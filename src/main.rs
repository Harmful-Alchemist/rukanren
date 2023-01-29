// From http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

// use crate::Term::{Object, Pair, Var};
use crate::Term::{Object, Var};
use std::cmp::Eq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Add;
use std::rc::Rc;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
enum Term<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    Var(T),
    // Pair(Box<Term<T>>, Box<Term<T>>),
    Object(T),
}

fn walk<T: Eq>(u: &Term<T>, s: Substitution<T>) -> Term<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    let mut pr = u;
    while s.contains_key(pr) {
        pr = s.get(pr).unwrap();
    }

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

type Stream<T> = dyn Iterator<Item = (Substitution<T>, T)>;

fn mzero<T>() -> Box<Stream<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    let vec: Vec<(Substitution<T>, T)> = Vec::new();
    Box::new(vec.into_iter())
}

type Goal<T> = dyn Fn((Substitution<T>, T)) -> Box<Stream<T>>;

// Returns a goal
fn eq<T>(u: Term<T>, v: Term<T>) -> impl Fn((Substitution<T>, T)) -> Box<Stream<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    move |(s, c): (Substitution<T>, T)| {
        if let Some(s2) = unify(&u, &v, s) {
            Box::new(vec![(s2, c)].into_iter())
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
        // (Pair(car_u, cdr_u), Pair(car_v, cdr_v)) => {
        //     if let Some(s1) = unify(car_u, car_v, s.clone()) {
        //         unify(cdr_u, cdr_v, s1)
        //     } else {
        //         None
        //     }
        // }
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
fn call_fresh<T>(
    f: impl Fn(Term<T>) -> Box<Goal<T>>,
) -> impl Fn((Substitution<T>, T)) -> Box<Stream<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    move |(s, c): (Substitution<T>, T)| f(Var(c.clone()))((s, c + 1))
}

//Returns a Goal
fn disj<T>(g1: Box<Goal<T>>, g2: Box<Goal<T>>) -> impl Fn((Substitution<T>, T)) -> Box<Stream<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    move |(s, c): (Substitution<T>, T)| mplus(g1((s.clone(), c.clone())), g2((s, c)))
}

fn conj<T>(
    g1: Box<Goal<T>>,
    g2: Rc<Box<Goal<T>>>,
) -> impl Fn((Substitution<T>, T)) -> Box<Stream<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    move |(s, c): (Substitution<T>, T)| bind(g1((s, c)), g2.clone()) // TODO was this but won't work with using g2.... Can't move..

    // move |(s, c): (Substitution<T>, T)| {
    //     let mut ret = Vec::new();
    //     // TODO not infinite
    //     for st in g1((s, c)) {
    //         let mut x = g2(st.clone());
    //         if !x.is_empty() {
    //             ret.append(&mut x);
    //         }
    //     }
    //     return ret;
    // }
}

struct DoubleIt<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    c: u32,
    s1: Box<Stream<T>>,
    s2: Box<Stream<T>>,
}

impl<T> Iterator for DoubleIt<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    type Item = (Substitution<T>, T);

    fn next(&mut self) -> Option<Self::Item> {
        println!("next from mplus");
        if self.c % 2 == 0 {
            if let Some(n) = self.s1.next() {
                return Some(n);
            }
        }
        self.s2.next()
    }
}

fn mplus<T>(st1: Box<Stream<T>>, st2: Box<Stream<T>>) -> Box<Stream<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    //infinite fine but I guess, problemsssssss.... With the eval model. COuld chain but doesn't matter, all the boxes....
    Box::new(DoubleIt {
        c: 0,
        s1: st1,
        s2: st2,
    })

    // // TODO not infinite
    // st1.append(&mut st2);
    // st1
}

fn bind<T>(mut st: Box<Stream<T>>, g: Rc<Box<Goal<T>>>) -> Box<Stream<T>>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    // TODO not infinite
    if let Some(x) = st.next() {
        mplus(g(x), bind(st, g))
    } else {
        Box::new(mzero())
    }
}

fn empty_state() -> (Substitution<i32>, i32) {
    (HashMap::new(), 0)
}

fn a_and_b() -> impl Fn((Substitution<i32>, i32)) -> Box<Stream<i32>> {
    conj(
        Box::new(call_fresh(|a| Box::new(eq(a, Object(7))))),
        Rc::new(Box::new(call_fresh(|b| {
            Box::new(disj(
                Box::new(eq(b.clone(), Object(5))),
                Box::new(eq(b.clone(), Object(6))),
            ))
        }))),
    )
}

fn fives(x: Term<i32>) -> impl Fn((Substitution<i32>, i32)) -> Box<Stream<i32>> {
    // println!("fives");
    //Ah crap execution, ..... Hmmmmmmmmm
    Box::new(disj(Box::new(eq(x.clone(), Object(5))), Box::new(fives(x))))
}

fn main() {
    let trying = call_fresh(|q| Box::new(eq(q, Object(5))))(empty_state());
    for t in trying {
        println!("Hello, world! {t:?}");
    }

    let pffff2 = a_and_b()(empty_state());
    for t in pffff2 {
        println!("Whee {t:?}");
    }

    // // Stack overfllllllllllow!
    let breaking = call_fresh(|x| Box::new(fives(x)))(empty_state());
    for t in breaking {
        println!("Break {t:?}")
    }
}
