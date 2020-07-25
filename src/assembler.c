#include "assembler.h"

#include <stdio.h>
#include <ctype.h>

typedef struct {
    enum {
        TOK_SPECIAL,
        TOK_STRING,
    } type;
    union {
        char element;
        char *str;
    };
} token_t;

static char asm_error_buf[128] = { 0 };

#define ASM_ERR(...) snprintf(asm_error_buf, sizeof(asm_error_buf) - 1, __VA_ARGS__)

const char *assembler_error(void)
{
    return asm_error_buf;
}

static bool is_separator(char c)
{
    return isspace(c) || c == ',';
}

static bool is_special(char c)
{
    static const char specials[] = "{}!-";
    const char *s = specials;
    while (*s)
        if (*s ++ == c)
            return true;
    return false;
}

static bool parseNum(const char *s, uint64_t *v)
{
    int radix = 10;
    uint64_t acc = 0;
    if (*s == '0') {
        radix = 8;
        switch (s[1]) {
        case 'x':
        case 'X': radix = 16; break;
        case 'o':
        case 'O': radix = 8; break;
        case 'b':
        case 'B': radix = 2; break;
        default: radix = 10; break;
        }
    }
    while (*s && !is_separator(*s)) {
        int c = *s;
        if (isdigit(c)) {
            c -= '0';
        } else if (isalpha(c)) {
            c -= isupper(c)? 'A' - 10 : 'a' - 10;
        } else
            return false;
        if (c >= radix)
            return false;
        acc *= radix;
        acc += c;
    }
    return true;
}

static int tokenizer(char *s, token_t args[])
{
    int idx = 0;
    while (*s) {
        if (isspace(*s)) {
            s++;
        } else if (is_special(*s)) {
            args[idx].type = TOK_SPECIAL;
            args[idx].element = *s;
            *s++ = 0;
            idx++;
        } else if (is_separator(*s)) {
            *s++ = 0;
            while (is_separator(*s))
                s++;
            args[idx].type = TOK_STRING;
            args[idx].str = s;
            idx++;
        } else
            s++;
    }
    return idx - 1;
}

bool assemble_line(char *line, uint32_t addr)
{
    static token_t params[20];
    ASM_ERR("ASM TODO");
    int n = tokenizer(line, params);
    if (n < 0) {
        ASM_ERR("parse error");
        return false;
    }
    for (int i=0; i<n; i++) {
        switch (params[i].type) {
        case TOK_STRING:
            printf("arg[%d]: <%s>\n", i, params[i].str);
            break;
        case TOK_SPECIAL:
            printf("arg[%d]: <%c>\n", i, params[i].element);
            break;
        }
    }
    return false;
}
