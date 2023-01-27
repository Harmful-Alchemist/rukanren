// From http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

use crate::Term::{Object, Pair, Var};
use std::cmp::Eq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Add;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
enum Term<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    Var(T),
    Pair(Box<Term<T>>, Box<Term<T>>),
    Object(T),
}

fn walk<T: Eq>(u: &Term<T>, s: Substitution<T>) -> Term<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    let mut pr = u;
    while s.contains_key(pr) {
        pr = s.get(pr).unwrap();
    };

    return pr.clone();
}

type Substitution<T> = HashMap<Term<T>, Term<T>>;

fn ext_s<T>(x: Term<T>, v: Term<T>, mut s: Substitution<T>) -> Substitution<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    if let Var(_) = &x {
        // Doesn't allow multiple substitutions of single var....
        s.insert(x, v);
        return s;
    }
    panic!("Illegal");
}

type Stream<T> = Vec<(Substitution<T>, T)>;

fn mzero<T>() -> Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    Vec::new()
}

type Goal<T: Debug + Hash + Eq + Clone + Add<i32, Output = T>> =
    dyn Fn((Substitution<T>, T)) -> Stream<T>;

// Returns a goal
fn eq<T>(u: Term<T>, v: Term<T>) -> impl Fn((Substitution<T>, T)) -> Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    move |(s, c): (Substitution<T>, T)| {
        if let Some(s2) = unify(&u, &v, s) {
            vec![(s2, c)]
        } else {
            mzero()
        }
    }
}

fn unify<T>(u: &Term<T>, v: &Term<T>, s: Substitution<T>) -> Option<Substitution<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    let u = &walk(u, s.clone());
    let v = &walk(v, s.clone());
    match (u, v) {
        (Var(_), Var(_)) if u == v => Some(s),
        (Var(_), _) => Some(ext_s(u.clone(), v.clone(), s)),
        (_, Var(_)) => Some(ext_s(v.clone(), u.clone(), s)),
        (Pair(car_u, cdr_u), Pair(car_v, cdr_v)) => {
            if let Some(s1) = unify(car_u, car_v, s.clone()) {
                unify(cdr_u, cdr_v, s1)
            } else {
                None
            }
        }
        _ => {
            if u == v {
                Some(s)
            } else {
                None
            }
        }
    }
}

// returns a goal
fn call_fresh<T>(f: impl Fn(Term<T>) -> Box<Goal<T>>) -> impl Fn((Substitution<T>, T)) -> Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    move |(s, c): (Substitution<T>, T)| f(Var(c.clone()))((s, c + 1))
}

//Returns a Goal
fn disj<T>(g1: Box<Goal<T>>, g2: Box<Goal<T>>) -> impl Fn((Substitution<T>, T)) -> Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    move |(s, c): (Substitution<T>, T)| mplus(g1((s.clone(), c.clone())), g2((s, c)))
}

fn conj<T>(g1: Box<Goal<T>>, g2: Box<Goal<T>>) -> impl Fn((Substitution<T>, T)) -> Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    // move |(s, c): (Substitution<T>, T)| bind(g1((s, c)), g2) TODO was this but won't work with using g2.... Can't move..
    move |(s, c): (Substitution<T>, T)| {
        let mut ret = Vec::new();
        // TODO not infinite
        for st in g1((s, c)) {
            let mut x = g2(st.clone());
            if !x.is_empty() {
                ret.append(&mut x);
            }
        }
        return ret;
    }
}

fn mplus<T>(mut st1: Stream<T>, mut st2: Stream<T>) -> Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    // TODO not infinite
    st1.append(&mut st2);
    st1
}

fn bind<T>(mut st: Stream<T>, g: Box<Goal<T>>) -> Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    // TODO not infinite
    if st.is_empty() {
        mzero()
    } else {
        mplus(g(st.pop().unwrap()), bind(st, g))
    }
}

fn empty_state() -> (Substitution<i32>, i32) {
    (HashMap::new(), 0)
}

fn a_and_b() -> impl Fn((Substitution<i32>, i32)) -> Stream<i32> {
    conj(
        Box::new(call_fresh(|a| Box::new(eq(a, Object(7))))),
        Box::new(call_fresh(|b| {
            Box::new(disj(
                Box::new(eq(b.clone(), Object(5))),
                Box::new(eq(b.clone(), Object(6))),
            ))
        })),
    )
}

fn fives(x: Term<i32>) -> impl Fn((Substitution<i32>, i32)) -> Stream<i32> {
    Box::new(disj(Box::new(eq(x.clone(), Object(5))), Box::new(fives(x))))
}

fn main() {
    let trying = call_fresh(|q| Box::new(eq(q, Object(5))))(empty_state());
    println!("Hello, world! {trying:?}");
    let pffff2 = a_and_b()(empty_state());
    println!("Whee {pffff2:?}");

    // Stack overfllllllllllow!
    let breaking = call_fresh(|x| Box::new(fives(x)))(empty_state());
    println!("Break {breaking:?}")
}
