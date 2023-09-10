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
exp err, nil, tru, Env;
exp evalList(exp, exp);
exp eval(exp, exp);
void print(exp);
exp parse();
int equ(exp, exp);
exp appendPair(exp, exp, exp);

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

exp cons(exp car, exp cdr) { //TODO Better variable names?
    exp c;
    c.type = CONS;
    c.value.cons = stack + stackptr;
    stack[stackptr++] = car;
    stack[stackptr++] = cdr;
    if (stackptr >= STACK_SIZE) { // stack overflow
        printf("Error: Out of memory\n");
        abort();
    }
    return c;
}

exp car(exp ex) {
    if (ex.type == CONS || ex.type == CLOSURE) {
        return *ex.value.cons;
    }
    //Error handling //TODO
    return err;
}

exp cdr(exp ex) {
    if (ex.type == CONS || ex.type == CLOSURE) {
        return *(ex.value.cons+1);
    }
    //Error handling //TODO
    return err;
}

int not(exp ex) {
    return ex.type == NIL;
}

exp closure(exp arg, exp ex, exp env) {
    exp cl, e;
    cl.type = CLOSURE;
    e = equ(env, Env) ? nil : env;
    cl.value.closure = appendPair(arg, ex, e).value.cons;
    return cl;
}

exp appendPair(exp key, exp val, exp env) {
    return cons(cons(key, val), env);
}

exp fun_eval(exp ex, exp env) {
    return eval(car(evalList(ex, env)), env);
}

exp fun_quote(exp ex, exp _) {
    return car(ex);
}

exp fun_cons(exp ex, exp env) {
    return cons(car(ex), car(cdr(ex)));
}

exp fun_car(exp ex, exp env) {
    return car(car(evalList(ex, env)));
}

exp fun_cdr(exp ex, exp env) {
    return cdr(car(evalList(ex, env)));
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

exp fun_lessThan(exp ex, exp env) { //TODO Error handling raise exception if args not number)
    ex = evalList(ex, env);
    if (car(ex).value.number - car(cdr(ex)).value.number < 0) {
        return tru;
    } else {
        return nil;
    }
}

exp fun_eq(exp ex, exp env) {
    ex = evalList(ex, env);
    if(equ(car(ex), car(cdr(ex)))) {
        return tru;
    } else {
        return nil;
    }
}

exp fun_not(exp ex, exp env) {
    if (not(car(evalList(ex, env)))) {
        return tru;
    } else {
        return nil;
    }
}

exp fun_or(exp ex, exp env) {
    exp x = nil;
    while (ex.type != NIL && not(x = eval(car(ex), env))) {
        ex = cdr(ex);
    }
    return x;
}

exp fun_and(exp ex, exp env) {
    exp x = nil;
    while (ex.type != NIL && !not(x = eval(car(ex), env))) {
        ex = cdr(ex);
    }
    return x;
}

exp fun_cond(exp ex, exp env) {
    while (ex.type != NIL && not(eval(car(car(ex)), env))) {
        ex = cdr(ex);
    }
    return eval(car(cdr(car(ex))), env);
}

exp fun_if(exp ex, exp env) {
    if (not(eval(car(ex), env))) {
        return eval(car(cdr(cdr(ex))), env);
    } else {
        return eval(car(cdr(ex)), env);
    }
}

exp fun_lambda(exp ex, exp env) {
    return closure(car(ex), car(cdr(ex)), env);
}

exp fun_define(exp ex, exp env) {
    Env = appendPair(car(ex), eval(car(cdr(ex)), env), Env); //TODO
    return car(ex);
}

int equ(exp x, exp y) {
    if(x.type == ATOM && y.type == ATOM) {
        return x.value.atom == y.value.atom;
    } else if (x.type == NUMBER && y.type == NUMBER) {
        return x.value.number == y.value.number;
    } else if (x.type == CONS && y.type == CONS) {
        return x.value.cons == y.value.cons;
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

exp bind(exp arg, exp ex, exp env) {
    if (arg.type == NIL) {
        return env;
    } else if (arg.type == CONS) {
        exp e = appendPair(car(arg), car(ex), env);
        return bind(cdr(arg), cdr(ex), e);
    } else {
        return appendPair(arg, ex, env);
    }
}

exp reduce(exp fun, exp ex, exp env) {
    exp b = bind(car(car(fun)), evalList(ex, env), not(cdr(fun)) ? Env : cdr(fun));
    return eval(cdr(car(fun)), b);
}

exp apply (exp fun, exp ex, exp env) {
    if (fun.type == PRIMITIVE) {
        return fun.value.fun(ex, env);
    } else if (fun.type == CLOSURE) {
        return reduce(fun, ex, env);
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
        return apply(eval(car(ex), env), cdr(ex), env); /*call to the primitive function*/ //TODO
    } else if (ex.type == ATOM) {
        return assoc(ex, env);
    } else {
        return nil;
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
    if(seeing('(') || seeing(')') || seeing('\'')) {
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

exp quote() {
    return cons(atom("quote"), cons(Read(), nil));
}

exp parse() {
    if (*buf == '(') {
        return list();
    } if (*buf == '\'') {
        return quote();
    }else { // TODO implement quote and comments
        return atomic();
    }
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
    } else if (ex.type == CLOSURE) {
        printf("{lambda at %p}", ex.value.closure);
    } else if (ex.type == PRIMITIVE) {
        printf("<complied function at %p>", ex.value.fun);
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
        {"quote", fun_quote},
        {"eval", fun_eval},
        {"cons", fun_cons},
        {"car", fun_car},
        {"cdr", fun_cdr},
        {"<", fun_lessThan},
        {"eq?", fun_eq},
        {"not", fun_not},
        {"or", fun_or},
        {"and", fun_and},
        {"cond", fun_cond},
        {"if", fun_if},
        {"lambda", fun_lambda},
        {"define", fun_define},
        {0}
    };

    exp fun = {.type = PRIMITIVE};
    for(int i = 0; prim_fun[i].name; i++) {
        fun.value.fun = prim_fun[i].fun;
        Env = appendPair(atom(prim_fun[i].name), fun, Env);
    }
}

void gc() {
    stackptr = (Env.value.cons - stack) + 2;
}

int main() {
    err = atom("ERR");
    nil.type = NIL;
    tru = atom("#t");
    Env = appendPair(tru, tru, nil);
    initEnv();
    printf("Welcome to licp.c");
    while(1) {
        printf("\n> ");
        print(eval(Read(), Env));
        gc();
    }
    return 0;
}
