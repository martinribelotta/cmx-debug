/* Copyright (C) 2020 Martin Ribelotta.

   This file is part of tdebug.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include <stdint.h>
#include <string.h>

#define WEAK __attribute__((weak))
#define ALIAS(sym) __attribute__((weak, alias (sym)))
#define NORETURN __attribute__((noreturn))

extern int main(void);

void SystemInit(void) WEAK;
void Dummy_Handler(void);

extern uint32_t _sidata;
extern uint32_t _sdata;
extern uint32_t _edata;
extern uint32_t _sbss;
extern uint32_t _ebss;
extern uint32_t _estack;

void Reset_Handler(void) NORETURN;
void NMI_Handler(void) ALIAS("Dummy_Handler");
void HardFault_Handler(void) ALIAS("Dummy_Handler");
void MemManage_Handler(void) ALIAS("Dummy_Handler");
void BusFault_Handler(void) ALIAS("Dummy_Handler");
void UsageFault_Handler(void) ALIAS("Dummy_Handler");
void SVC_Handler(void) ALIAS("Dummy_Handler");
void DebugMon_Handler(void) ALIAS("Dummy_Handler");
void PendSV_Handler(void) ALIAS("Dummy_Handler");
void SysTick_Handler(void) ALIAS("Dummy_Handler");

/* 16 standard Cortex-M vectors - these are present in every MCU */
void *core_vector_table[] __attribute__ ((section(".isr_vectors"))) = {
   &_estack,
   Reset_Handler,
   NMI_Handler,
   HardFault_Handler,
   MemManage_Handler,
   BusFault_Handler,
   UsageFault_Handler,
   0,
   0,
   0,
   0,
   SVC_Handler,
   DebugMon_Handler,
   0,
   PendSV_Handler,
   SysTick_Handler,
};

static inline void copy32(uint32_t *dst, const uint32_t *src, const uint32_t *const end) __attribute__((always_inline));
static inline void copy32(uint32_t *dst, const uint32_t *src, const uint32_t *const end)
{
   while (dst < end)
      *dst++ = *src++;   
}

static inline void zero32(uint32_t *dst, const uint32_t *const end) __attribute__((always_inline));
static inline void zero32(uint32_t *dst, const uint32_t *const end)
{
   while (dst < end)
      *dst++ = 0;
}

void Reset_Handler(void)
{
   __asm__ volatile("cpsie i");
   copy32(&_sdata, &_sidata, &_edata);
   zero32(&_sbss, &_ebss);
   SystemInit();
   main();
   for (;;) {
      __asm__ volatile("wfi");
   }
}

void Dummy_Handler(void)
{
   for(;;) {}
}

void SystemInit(void) {}