#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STACK_SIZE 1024
#define HEAP_SIZE 1024

typedef struct exp {
    enum {
        NUMBER,
        ATOM,
        STRING,
        PRIMITIVE,
        CONS,
        CLOSURE,
        NIL
    } type;

    union {
        double number;
        const char *atom;
        const char *string;
        struct exp (*fun)(struct exp, struct exp);
        struct exp *cons;
        struct exp *closure;
    } value;
} exp;

exp stack[STACK_SIZE];
char heap[HEAP_SIZE];
int stackptr = 0;
int heapptr = 0;
exp err, nil, tru, env;
exp evalList(exp, exp);
exp eval(exp, exp);
void print(exp);
exp parse();

exp atom(const char *str) {
    int ptr = 0;
    while (ptr < heapptr && strcmp(heap + ptr, str)) {
        ptr += strlen(heap + ptr)+1;
    }
    if(ptr == heapptr) {
        heapptr += strlen(strcpy(heap + ptr, str))+1;
    }
    if(heapptr >= HEAP_SIZE) {
        abort();
    }
    exp at;
    at.type = ATOM;
    at.value.atom = heap + ptr;
    return at;
}

exp cons(exp car, exp cdr) {
    exp c;
    c.type = CONS;
    c.value.cons = stack + stackptr;
    stack[stackptr++] = car;
    stack[stackptr++] = cdr;
    if (stackptr >= STACK_SIZE) {
        printf("Error: Out of memory\n");
        abort();
    }
    return c;
}

exp car(exp ex) {
    if (ex.type == CONS) {
        return *ex.value.cons;
    }
    //Error handling //TODO
    return err;
}

exp cdr(exp ex) {
    if (ex.type == CONS) {
        return *(ex.value.cons+1);
    }
    //Error handling //TODO
    return err;
}

int not(exp ex) {
    return ex.type == NIL;
}

exp appendPair(exp key, exp val, exp env) {
    return cons(cons(key, val), env);
}

exp fun_add(exp ex, exp env) {
    exp n;
    ex = evalList(ex, env);
    n = car(ex);
    ex = cdr(ex);
    while(!not(ex)) {
        n.value.number += car(ex).value.number;
        ex = cdr(ex);
    }
    return n;
}

exp fun_sub(exp ex, exp env) {
    exp n;
    ex = evalList(ex, env);
    n = car(ex);
    ex = cdr(ex);
    while(!not(ex)) {
        n.value.number -= car(ex).value.number;
        ex = cdr(ex);
    }
    return n;
}

exp fun_mul(exp ex, exp env) {
    exp n;
    ex = evalList(ex, env);
    n = car(ex);
    ex = cdr(ex);
    while(!not(ex)) {
        n.value.number *= car(ex).value.number;
        ex = cdr(ex);
    }
    return n;
}

exp fun_div(exp ex, exp env) {
    exp n;
    ex = evalList(ex, env);
    n = car(ex);
    ex = cdr(ex);
    while(!not(ex)) {
        n.value.number /= car(ex).value.number;
        ex = cdr(ex);
    }
    return n;
}


void printCons(exp ex) {
    putchar('(');
    while(ex.type == CONS) {
        print(car(ex));
        ex = cdr(ex);
        if(ex.type == NIL) {
            break;
        }
        if (ex.type != CONS) {
            printf(" . ");
            print(ex);
            break;
        }
        putchar(' ');
    }
    putchar(')');
}

void print(exp ex) {
    if (ex.type == NIL) {
        printf("()");
    } else if (ex.type == NUMBER) {
        printf("%.16lg", ex.value.number);
    } else if (ex.type == ATOM) {
        printf("%s", ex.value.string);
    } else if (ex.type == CONS) {
        printCons(ex);
    }
}

int equ(exp x, exp y) {
    if(x.type == ATOM && y.type == ATOM) {
        return x.value.atom == y.value.atom;
    }
    return 0;
}

exp assoc(exp ex, exp env) {
    while (env.type == CONS && !equ(ex, car(car(env)))) {
        env = cdr(env);
    }
    if(env.type == CONS) {
        return cdr(car(env));
    } else {
        return err;
    }
}

exp apply (exp fun, exp ex, exp env) {
    if (fun.type == PRIMITIVE) {
        return fun.value.fun(ex, env);
    } else {
        return err;
    }
}

exp evalList(exp ex, exp env) {
    if (ex.type == CONS) {
        return cons(eval(car(ex), env), evalList(cdr(ex), env));
    } else if (ex.type == ATOM) {
        return assoc(ex, env);
    } else {
        return nil;
    }
}

exp eval(exp ex, exp env) {
    if (ex.type == NUMBER) {
        return ex;
    } else if (ex.type == CONS) {
        return apply(eval(car(ex), env), cdr(ex), env);
    } else if (ex.type == ATOM) {
        return assoc(ex, env);
    }
}

char buf[40], see=' ';

void look() {
    int c = getchar();
    see = c;
    if (c == EOF) {
        exit(0);
    }
}

int seeing(char c) {
    if (c == ' ') {
        return see > 0 && see <= c;
    } else {
        return see == c;
    }
}

char get() {
    char c = see;
    look();
    return c;
}

char scan() {
    int i = 0;
    while (seeing(' ')) {
        look();
    }
    if(seeing('(') || seeing(')')) {
        buf[i++] = get();
    } else {
        do {
            buf[i++] = get();
        } while(i < 39 && !seeing('(') && !seeing(')') && !seeing(' '));
    }
    buf[i] = 0;
    return *buf;
}

exp Read() {
    scan();
    return parse();
}

exp list() {
    exp ex;
    if (scan() == ')') {
        return nil;
    } if (!strcmp(buf, ".")) {
        ex = Read();
        scan();
        return ex;
    }
    ex = parse();
    return cons(ex, list());
}

exp atomic() {
    exp n;
    int i;
    if(sscanf(buf, "%lg%n", &n.value.number, &i) > 0 && !buf[i]) {
        n.type = NUMBER;
        return n;
    } else {
        return atom(buf);
    }
}

exp parse() {
    if( *buf == '(') {
        return list();
    } else { // TODO implement quote and comments
        return atomic();
    }
}

void initEnv() {
    struct {
        const char *name;
        exp (*fun)(exp, exp);
    } prim_fun[] = {
        {"+", fun_add},
        {"-", fun_sub},
        {"*", fun_mul},
        {"/", fun_div},
        {0}
    };

    exp fun = {.type = PRIMITIVE};
    for(int i = 0; prim_fun[i].name; i++) {
        fun.value.fun = prim_fun[i].fun;
        env = appendPair(atom(prim_fun[i].name), fun, env);
    }
}

int main() {
    err = atom("ERR");
    nil.type = NIL;
    tru = atom("#t");
    env = appendPair(tru, tru, nil);
    initEnv();
    printf("Welcome to licp.c");
    while(1) {
        printf("\n> ");
        print(eval(Read(), env));
    }

    return 0;
}
