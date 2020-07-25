#ifndef __ASSEMBLER_H__
#define __ASSEMBLER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

extern bool assemble_line(char *line, uint32_t addr);
extern const char *assembler_error(void);

#ifdef __cplusplus
}
#endif

#endif /* __ASSEMBLER_H__ */
