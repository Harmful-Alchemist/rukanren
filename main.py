def empty_state():
    return ([], 0)


# Var
def var(c):
    return [c]


def is_var(m_var):
    return isinstance(m_var, list)


def var_eq(a, b):
    return a[0] == b[0]


#
def walk(u, s):
    print(f"{u=}  {s=}")
    if not is_var(u):
        return u

    pr = filter(lambda x: var_eq(x, u), s)

    if pr and len(list(pr)) > 0:
        return list(pr)
    else:
        return u


def ext_s(x, v, s):
    return [(x, v)] + s


def mzero():
    return []


def unify(u, v, s):
    u = walk(u, s)
    v = walk(v, s)

    if is_var(u) and is_var(v) and var_eq(u, v):
        return s
    elif is_var(u):
        return ext_s(u, v, s)
    elif is_var(v):
        return ext_s(v, u, s)
    else:
        if u == v:
            return s
        else:
            return []


def eq(u, v):
    # sc is a tuple/pair
    def f(sc):
        s = unify(u, v, sc[0])
        if s:
            return [(s, sc[1])] + []
        else:
            return mzero()

    return f


def call_fresh(f):
    def cf(sc):
        c = sc[1]
        return f(var(c))((sc[0], c + 1))

    return cf


def mplus(s1, s2):
    # Should be iterator!
    return s1 + s2


def bind(s, g):
    if len(s) == 0:
        return mzero()
    else:
        return mplus(g(s[0]), bind(s[1:], g))


def disj(g1, g2):
    def lg(sc):
        return mplus(g1(sc), g2(sc))

    return lg


def conj(g1, g2):
    def lg(sc):
        return bind(g1(sc), g2)

    return lg


def a_and_b(state):
    return conj(
        call_fresh(lambda a:
                   eq(a, 7)),
        call_fresh(lambda b: disj(
            eq(b, 5),
            eq(b, 6)
        )))(state)

def fives(x):
    disj(
        eq(x,5),
        fives(x)
    )

if __name__ == '__main__':
    print(a_and_b(empty_state()))

#     Lets make it go wrong - RecursionError: maximum recursion depth exceeded lol
#     print(call_fresh(fives)(empty_state()))