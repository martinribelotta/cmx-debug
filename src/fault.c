#include "fault.h"
#include "ucmsis.h"

#include <stdio.h>

typedef struct {
   uint32_t r4;
   uint32_t r5;
   uint32_t r6;
   uint32_t r7;
   uint32_t r8;
   uint32_t r9;
   uint32_t r10;
   uint32_t r11;
   uint32_t r0;
   uint32_t r1;
   uint32_t r2;
   uint32_t r3;
   uint32_t r12;
   uint32_t lr;
   uint32_t pc;
   uint32_t xpsr;
} context_t;

context_t jmp_ctx;
uint32_t saved_sp;

static inline void handle_trampoline(void (*func)(context_t *ctx, uint32_t lr)) __attribute__((always_inline));
static inline void handle_trampoline(void (*func)(context_t *ctx, uint32_t lr))
{
   __asm__ volatile (
      "  tst lr, #4\n"
      "  ite  EQ\n"
      "  mrseq r0, MSP\n"
      "  mrsne r0, PSP\n"
      "  stmdb r0!, {r4-r11}\n"
      "  mov sp, r0\n"
      "  mov r1, lr\n"
      "  b %[ptr]\t\n"
      : /* no output */
      : [ptr] "i" (func) /* input */
      : "r0" /* clobber */
   );
}

static void int_setjmp(void) __attribute((naked));
static void int_setjmp(void)
{
   __asm__ volatile(
      "           ldr r0, =ret_label\n"
      "           ldr r1, =jmp_ctx\n"
      "           ldr r3, =saved_sp\n"
      "           str sp, [r3]\n"
      "           svc #0x55\n"
      "ret_label: ldr sp, [r3]\n"
      "           bx lr\n"
   );
}

void fault_init(void)
{
   SCB->SHCSR |= SCB_SHCSR_USGFAULTENA_Msk |
                 SCB_SHCSR_BUSFAULTENA_Msk |
                 SCB_SHCSR_MEMFAULTENA_Msk;
   int_setjmp();
}

void HardFault_Handler(void)
{
   printf("HARD FAULT\n");
   while(1) {}
}

void MemManage_Handler(void)
{
   printf("MPU FAULT\n");
   while(1) {}
}

static void bf_call_handler_main(context_t *ctx, uint32_t lr)
{
#define _(r) printf(#r "\t=%08X\n", ctx->r)
   _(r0);
   _(r1);
   _(r2);
   _(r3);
   _(r4);
   _(r5);
   _(r6);
   _(r7);
   _(r8);
   _(r9);
   _(r10);
   _(r11);
   _(r12);
   _(lr);
   _(pc);
   _(xpsr);
#undef _
   printf("BUS FAULT\n");
   union {
      struct {
         uint8_t IBUSERR: 1;
         uint8_t PRECISERR: 1;
         uint8_t IMPRECISERR: 1;
         uint8_t UNSTKERR: 1;
         uint8_t STKERR: 1;
         uint8_t LSPERR: 1;
         uint8_t reserv: 1;
         uint8_t BFARVALID: 1;
      };
      uint8_t d;
   } bfsr;
   bfsr.d = (SCB->CFSR >> 8) & 0xFF;
   printf(
         "IBUSERR=%d\n"
         "PRECISERR=%d\n"
         "IMPRECISERR=%d\n"
         "UNSTKERR=%d\n"
         "STKERR=%d\n"
         "LSPERR=%d\n"
         "BFARVALID=%d\n",
         bfsr.IBUSERR,
         bfsr.PRECISERR,
         bfsr.IMPRECISERR,
         bfsr.UNSTKERR,
         bfsr.STKERR,
         bfsr.LSPERR,
         bfsr.BFARVALID
   );
   if (bfsr.BFARVALID) {
      printf("at addr 0x%08X\n", SCB->BFAR);
   }
   __asm__ volatile("mov lr, %1\n"
                    "mov sp, %0\n"
                    "pop {r4-r11}\n"
                    "bx lr\n": : "r" (&jmp_ctx), "r" (lr) : "r0");
}

void BusFault_Handler(void) __attribute((naked));
void BusFault_Handler(void)
{
   handle_trampoline(bf_call_handler_main);
}

void UsageFault_Handler(void)
{
   printf("USAGE FAULT\n");
   while(1) {}
}

void SVC_Handler(void)
{
   register context_t *ctx __asm__("r0");
   asm volatile(
      "tst lr, #4\t\n" /* Check EXC_RETURN[2] */
      "ite eq\t\n"
      "mrseq r0, msp\t\n"
      "mrsne r0, psp\t\n"
      "stmdb r0!, {r4-r11}\n"
   );
   unsigned int svc_number = ((char *)ctx->pc)[-2];
   switch (svc_number) {
   case 0x55:
      jmp_ctx = *ctx;
      break;
   default:
      break;
   }
}
/*
void DebugMon_Handler(void)
{
   printf("DEBUG FAULT\n");
   while(1) {}
}
*/