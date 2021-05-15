#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage() {
    fprintf(stderr, "usage: arith ( -small-step | -big-step ) file\n\n");
    fprintf(stderr, "arith is an implementation of the untyped calculus\n");
    fprintf(stderr, "of booleans and numbers (TAPL chapter 3 & 4).\n");
    exit(2);
}

typedef enum {
    tEOF,
    tTrue,
    tFalse,
    tIf,
    tThen,
    tElse,
    tZero,
    tSucc,
    tPred,
    tIsZero,
} token_t;

static const char *token_string[] = {
    "EOF",
    "true",
    "false",
    "if",
    "then",
    "else",
    "0",
    "succ",
    "pred",
    "iszero",
};

typedef struct {
    int i;
    int ch;
    char buf[6];
    FILE *f;
} scanner_t;

void errExit(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    exit(1);
}

void invalidToken(scanner_t *s) {
    fputc('"', stderr);
    fwrite(s->buf, 1, s->i, stderr);
    while (s->ch != EOF && !isspace(s->ch)) {
        fputc(s->ch, stderr);
        s->ch = fgetc(s->f);
    }
    errExit("\" is not a valid token\n");
}

token_t getTokenType(scanner_t *s) {
    for (token_t t = 0; t < sizeof(token_string) / sizeof(char *); t++) {
        if (!strncmp(token_string[t], s->buf, s->i)) {
            return t;
        }
    }
    invalidToken(s);
}

token_t scan(scanner_t *s) {
    do {
        s->ch = fgetc(s->f);
        if (s->ch == EOF || isspace(s->ch)) {
            if (s->i > 0) {
                token_t t = getTokenType(s);
                s->i = 0;
                return t;
            }
            continue;
        }
        if (s->i == 6) invalidToken(s);
        s->buf[s->i++] = s->ch;
    } while (s->ch != EOF);
    if (ferror(s->f) != 0) errExit(strerror(errno));
    return tEOF;
}

typedef enum {
    tmTrue,
    tmFalse,
    tmZero,
    tmSucc,
    tmPred,
    tmIsZero,
    tmIf,
} tmType_t;

static const char *tmType_string[] = {
    "true",
    "false",
    "0",
    "succ",
    "pred",
    "iszero",
    "if",
};

typedef struct term {
    tmType_t tmType;
    struct term *children[3];
} term_t;

int childCount(term_t *t) {
    if (t->tmType == tmIf) return 3;
    if (t->tmType >= tmSucc) return 1;
    return 0;
}

void expect(scanner_t *s, token_t want) {
    token_t got = scan(s);
    if (got != want) {
        errExit("expected token \"%s\", got \"%s\"n\n", token_string[want], token_string[got]);
    }
}

term_t *newTerm(tmType_t tmType, term_t *t1, term_t *t2, term_t *t3) {
    term_t *t = malloc(sizeof(term_t));
    t->tmType = tmType;
    t->children[0] = t1;
    t->children[1] = t2;
    t->children[2] = t3;
    return t;
}

term_t *parse(scanner_t *s) {
    token_t t;
    term_t *t1, *t2, *t3;
    while ((t = scan(s)) != tEOF) {
        switch (t) {
        case tTrue: return newTerm(tmTrue, NULL, NULL, NULL);
        case tFalse: return newTerm(tmFalse, NULL, NULL, NULL);
        case tZero: return newTerm(tmZero, NULL, NULL, NULL);
        case tSucc: return newTerm(tmSucc, parse(s), NULL, NULL);
        case tPred: return newTerm(tmPred, parse(s), NULL, NULL);
        case tIsZero: return newTerm(tmIsZero, parse(s), NULL, NULL);
        case tIf:
            t1 = parse(s);
            expect(s, tThen);
            t2 = parse(s);
            expect(s, tElse);
            t3 = parse(s);
            return newTerm(tmIf, t1, t2, t3);
        default: goto unexpected;
        }
    }
unexpected:
    errExit("unexpected token \"%s\"\n", token_string[t]);
}

char *cat(char *a, char *b) {
    size_t a_size = strlen(a);
    size_t b_size = strlen(b);
    char *c = malloc(a_size + b_size + 1);
    memcpy(c, a, a_size);
    memcpy(c + a_size, b, b_size + 1);
    return c;
}

void printChildren(char *indent, int count, term_t *children[3]) {
    term_t *t;
    char *indentA = cat(indent, "  ");
    char *indentB = cat(indent, "│ ");
    for (int i = 0; i < count; i++) {
        t = children[i];
        if (i == (count - 1)) {
            printf("%s└─%s\n", indent, tmType_string[t->tmType]);
            printChildren(indentA, childCount(t), t->children);
        } else {
            printf("%s├─%s\n", indent, tmType_string[t->tmType]);
            printChildren(indentB, childCount(t), t->children);
        }
    }
    free(indentA);
    free(indentB);
}

void printTerm(term_t *t) {
    printf("%s\n", tmType_string[t->tmType]);
    printChildren("", childCount(t), t->children);
}

bool isNumericVal(term_t *t) {
    switch (t->tmType) {
    case tmZero: return true;
    case tmSucc: return isNumericVal(t->children[0]);
    default: return false;
    }
}

term_t *eval1(bool *noRuleApplies, term_t *t) {
    term_t *t1Prime, *res;
    term_t *t1 = t->children[0];
    switch (t->tmType) {
    case tmIf:
        if (t1->tmType == tmTrue) return t->children[1];
        if (t1->tmType == tmFalse) return t->children[2];
        t1Prime = eval1(noRuleApplies, t1);
        if (*noRuleApplies) return res;
        return newTerm(tmIf, t1Prime, t->children[1], t->children[2]);
    case tmSucc:
        t1Prime = eval1(noRuleApplies, t1);
        if (*noRuleApplies) return res;
        return newTerm(tmSucc, t1Prime, NULL, NULL);
    case tmPred:
        if (t1->tmType == tmZero) return newTerm(tmZero, NULL, NULL, NULL);
        if (t1->tmType == tmSucc && isNumericVal(t1->children[0])) return t1->children[0];
        t1Prime = eval1(noRuleApplies, t1);
        if (*noRuleApplies) return res;
        return newTerm(tmPred, t1Prime, NULL, NULL);
    case tmIsZero:
        if (t1->tmType == tmZero) return newTerm(tmTrue, NULL, NULL, NULL);
        if (t1->tmType == tmSucc && isNumericVal(t1->children[0])) return newTerm(tmFalse, NULL, NULL, NULL);
        t1Prime = eval1(noRuleApplies, t1);
        if (*noRuleApplies) return res;
        return newTerm(tmIsZero, t1Prime, NULL, NULL);
    default:
        *noRuleApplies = true;
        return res;
    }
}

term_t *evalSmallStep(term_t *t) {
    bool noRuleApplies = false;
    term_t *t1Prime = eval1(&noRuleApplies, t);
    if (noRuleApplies) return t;
    return evalSmallStep(t1Prime);
}

bool isVal(term_t *t) {
    switch (t->tmType) {
    case tmTrue:
    case tmFalse:
        return true;
    default:
        return isNumericVal(t);
    }
}

term_t *evalBigStep(term_t *t) {
    if (isVal(t) || t->tmType <= tmZero) return t;
    term_t *v1 = evalBigStep(t->children[0]);
    switch (t->tmType) {
    case tmIf:
        switch (v1->tmType) {
        case tmTrue: return evalBigStep(t->children[1]);
        case tmFalse: return evalBigStep(t->children[2]);
        }
    case tmSucc:
        if (isNumericVal(v1)) return newTerm(tmSucc, v1, NULL, NULL);
    case tmPred:
        if (v1->tmType == tmZero || v1->tmType == tmSucc) return v1;
    case tmIsZero:
        switch (v1->tmType) {
        case tmZero: return newTerm(tmTrue, NULL, NULL, NULL);
        case tmSucc: return newTerm(tmFalse, NULL, NULL, NULL);
        }
    }
}

int main(int argc, char const *argv[]) {
    if (argc != 3) usage();
    bool smallStep = !strcmp(argv[1], "-small-step") || !strcmp(argv[2], "-small-step");
    bool bigStep = !strcmp(argv[1], "-big-step") || !strcmp(argv[2], "-big-step");
    if (smallStep == bigStep) usage();
    FILE *f = fopen(argv[2], "rb");
    if (!f) {
        fprintf(stderr, "open %s: %s\n", argv[2], strerror(errno));
        usage();
    }
    scanner_t s = {.buf = {0}, .i = 0, .f = f};
    term_t *ast = parse(&s);
    if (smallStep) {
        printTerm(evalSmallStep(ast));
    } else {
        printTerm(evalBigStep(ast));
    }
    return (bool)fclose(f);
}
