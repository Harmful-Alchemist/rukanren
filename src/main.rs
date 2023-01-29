// From http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

use std::cell::RefCell;
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
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    Var(T),
    // Pair(Box<Term<T>>, Box<Term<T>>),
    Object(T),
}

trait Stream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: T,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, T)>>;
}

fn walk<T: Eq>(u: &Term<T>, s: Substitution<T>) -> Term<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    let mut pr = u;
    while s.contains_key(pr) {
        pr = s.get(pr).unwrap();
    }

    return pr.clone();
}

type Substitution<T> = HashMap<Term<T>, Term<T>>;

fn ext_s<T>(x: Term<T>, v: Term<T>, s: &mut Substitution<T>)
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    if let Var(_) = &x {
        // Doesn't allow multiple substitutions of single var....
        s.insert(x, v);
    } else {
        panic!("Illegal");
    }
}

struct EqStream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    u: Term<T>,
    v: Term<T>,
    s: Substitution<T>,
    c: T,
    done: bool,
}

impl<T> Iterator for EqStream<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    type Item = (Substitution<T>, T);

    fn next(&mut self) -> Option<Self::Item> {
        // println!("Eq next!");
        if !self.done && unify(&self.u, &self.v, &mut self.s) {
            self.done = true;
            Some((self.s.clone(), self.c.clone()))
        } else {
            None
        }
    }
}

struct CallFresh<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    obj: Term<T>,
}

impl<T> CallFresh<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    fn new(t: Term<T>) -> Self {
        if let Object(o) = t {
            Self { obj: Object(o) }
        } else {
            panic!("Illegal")
        }
    }
}

impl<T> Stream<T> for CallFresh<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: T,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, T)>> {
        Box::new(EqStream {
            u: Var(c.clone()),
            v: (&self.obj).clone(),
            s,
            c: c.clone() + 1,
            done: false,
        })
    }
}

struct Disj<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    g1: Rc<Box<dyn Stream<T>>>,
    g2: Rc<Box<dyn Stream<T>>>,
}

impl<T> Stream<T> for Disj<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: T,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, T)>> {
        //TODO probably interleave
        let x = self.g1.from_state(s.clone(), c.clone());
        let y = self.g2.from_state(s.clone(), c.clone());
        Box::new(x.chain(y))
    }
}

struct Conj<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    g1: Rc<Box<dyn Stream<T>>>,
    g2: Rc<Box<dyn Stream<T>>>,
}

impl<T> Stream<T> for Conj<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: T,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, T)>> {
        //TODO probably interleave
        let x = self.g1.from_state(s.clone(), c.clone());
        Box::new(ConjIterator {
            s: x,
            g: self.g2.clone(),
            next: None,
        })
    }
}

struct ConjIterator<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    s: Box<dyn Iterator<Item = (Substitution<T>, T)>>,
    g: Rc<Box<dyn Stream<T>>>,
    next: Option<RefCell<Box<dyn Iterator<Item = (Substitution<T>, T)>>>>,
}

impl<T> Iterator for ConjIterator<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    type Item = (Substitution<T>, T);

    fn next(&mut self) -> Option<Self::Item> {
        // println!("Conj next!");
        if let Some(s2) = &self.next {
            let new_stream = s2.borrow_mut().next();
            if let Some(s) = new_stream {
                return Some(s);
            }
        } else {
            if let Some((s_, c_)) = self.s.next() {
                self.next = Some(RefCell::from(self.g.from_state(s_.clone(), c_.clone())));
                return (&self.next.as_ref().unwrap().borrow_mut().next()).clone();
            }
        }

        None
    }
}

fn main() {
    println!("wtf");
    let (s, c) = empty_state();

    let disj = Disj {
        g1: Rc::new(Box::new(CallFresh::new(Object(5)))),
        g2: Rc::new(Box::new(CallFresh::new(Object(6)))),
    };

    for s in disj.from_state(s.clone(), c.clone()) {
        println!("snd: {s:?}")
    }

    let conj = Conj {
        g1: Rc::new(Box::new(CallFresh::new(Object(7)))),
        g2: Rc::new(Box::new(Disj {
            g1: Rc::new(Box::new(CallFresh::new(Object(5)))),
            g2: Rc::new(Box::new(CallFresh::new(Object(6)))),
        })),
    };

    for s in conj.from_state(s.clone(), c.clone()) {
        println!("trd: {s:?}");
    }

    let fives = Fives {
        x: Object(5),
    };
    for s in fives.from_state(s.clone(),c.clone()) {
        // println!("frt {s:?}");
        let size = s.0.len();
        let count = s.1;
        println!("length: {size} count: {count}");
        println!("=======================================================");
        // break
    }
}

struct Fives<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    x: Term<T>,
}

impl<T> Stream<T> for Fives<T>
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: T,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, T)>> {
        Box::new(FivesIterator::new(self.x.clone(), s, c))
    }
}

struct FivesIterator<T> where
T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,{
    x: Term<T>,
    next: RefCell<Box<dyn Iterator<Item = (Substitution<T>, T)>>>
}

impl<T> FivesIterator<T>
    where
        T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,{
        fn new(x: Term<T>, s: Substitution<T>, c: T) -> Self {
            let cf = CallFresh {
                obj: x.clone()
            };
            Self {
                x,
                next: RefCell::new(cf.from_state(s,c)),
            }
        }
        }

impl<T> Iterator for FivesIterator<T>
    where
        T: Debug + Hash + Eq + Clone + Add<i32, Output = T> + 'static,{
    type Item = (Substitution<T>, T);

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next.get_mut().next();
        if let Some((s,c)) = next {
            let cf = CallFresh{obj: self.x.clone()};
            self.next = RefCell::new(cf.from_state(s.clone(),c.clone()));
            return Some((s,c))
        }
        None
    }
}



fn unify<T>(u: &Term<T>, v: &Term<T>, s: &mut Substitution<T>) -> bool
where
    T: Debug + Hash + Eq + Clone + Add<i32, Output = T>,
{
    let u = &walk(u, s.clone());
    let v = &walk(v, s.clone());
    match (u, v) {
        (Var(_), Var(_)) if u == v => true,
        (Var(_), _) => {
            ext_s(u.clone(), v.clone(), s);
            true
        }
        (_, Var(_)) => {
            ext_s(v.clone(), u.clone(), s);
            true
        }
        // (Pair(car_u, cdr_u), Pair(car_v, cdr_v)) => {
        //     if let Some(s1) = unify(car_u, car_v, s.clone()) {
        //         unify(cdr_u, cdr_v, s1)
        //     } else {
        //         false
        //     }
        // }
        _ => {
            if u == v {
                true
            } else {
                false
            }
        }
    }
}

fn empty_state() -> (Substitution<i32>, i32) {
    (HashMap::new(), 0)
}
