// From http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

// use crate::Term::{Object, Pair, Var};
use crate::Term::{Object, Var};
use std::cell::RefCell;
use std::cmp::Eq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::Map;
use std::ops::Add;
use std::rc::Rc;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
enum Term<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    Var(i32),
    // TODO pairs.
    // Pair(Box<Term<T>>, Box<Term<T>>),
    Object(T),
}

trait Stream<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, i32)>>;
}

fn walk<T: Eq>(u: &Term<T>, s: Substitution<T>) -> Term<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    let mut pr = u;
    if s.contains_key(pr) {
        pr = s.get(pr).unwrap();
    }

    return pr.clone();
}

type Substitution<T> = HashMap<Term<T>, Term<T>>;

fn ext_s<T>(x: Term<T>, v: Term<T>, s: &mut Substitution<T>)
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    if let Var(_) = &x {
        // Doesn't allow multiple substitutions of single var....
        s.insert(x, v);
    } else {
        panic!("Illegal");
    }
}

struct Eq_<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    t: Term<T>,
}

impl<T> Stream<T> for Eq_<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, i32)>> {
        Box::new(EqIterator {
            u: Var(c.clone()),
            v: (&self.t).clone(),
            s,
            c: c.clone(),
            done: false,
        })
    }
}

struct EqIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    u: Term<T>,
    v: Term<T>,
    s: Substitution<T>,
    c: i32,
    done: bool,
}

impl<T> Iterator for EqIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    type Item = (Substitution<T>, i32);

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
    T: Debug + Hash + Eq + Clone + 'static,
{
    s: Box<dyn Stream<T>>,
}

impl<T> Stream<T> for CallFresh<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, i32)>> {
        Box::new(self.s.from_state(s, c + 1))
    }
}

struct Disj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    g1: Rc<Box<dyn Stream<T>>>,
    g2: Rc<Box<dyn Stream<T>>>,
}

impl<T> Stream<T> for Disj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, i32)>> {
        let x = self.g1.from_state(s.clone(), c.clone());
        let y = self.g2.from_state(s.clone(), c.clone());
        Box::new(DisjIterator { m: 0, g1: x, g2: y })
    }
}

struct DisjIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    m: usize,
    g1: Box<dyn Iterator<Item = (Substitution<T>, i32)>>,
    g2: Box<dyn Iterator<Item = (Substitution<T>, i32)>>,
}

impl<T> Iterator for DisjIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    type Item = (Substitution<T>, i32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.m % 2 == 0 {
            // println!("1st");
            let opt = self.g1.next();
            if !opt.is_none() {
                self.m = self.m + 1;
                return opt;
            }
        }
        self.m = self.m + 1;
        // println!("2nd");
        self.g2.next()
    }
}

struct Conj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    g1: Rc<Box<dyn Stream<T>>>,
    g2: Rc<Box<dyn Stream<T>>>,
}

impl<T> Stream<T> for Conj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, i32)>> {
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
    T: Debug + Hash + Eq + Clone + 'static,
{
    s: Box<dyn Iterator<Item = (Substitution<T>, i32)>>,
    g: Rc<Box<dyn Stream<T>>>,
    next: Option<RefCell<Box<dyn Iterator<Item = (Substitution<T>, i32)>>>>,
}

impl<T> Iterator for ConjIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    type Item = (Substitution<T>, i32);

    fn next(&mut self) -> Option<Self::Item> {
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

fn unify<T>(u: &Term<T>, v: &Term<T>, s: &mut Substitution<T>) -> bool
where
    T: Debug + Hash + Eq + Clone,
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
        //     if unify(car_u, car_v, s) {
        //         unify(cdr_u, cdr_v, s)
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

fn main() {
    let (s, c) = empty_state();

    let disj = CallFresh {
        s: Box::new(Disj {
            g1: Rc::new(Box::new(Eq_ { t: Object(5) })),
            g2: Rc::new(Box::new(Eq_ { t: Object(6) })),
        }),
    };

    for s in disj.from_state(s.clone(), c.clone()) {
        println!("snd: {s:?}")
    }

    Rc::new(Box::new(CallFresh {
        s: Box::new(Eq_ { t: Object(7) }),
    }));

    let conj = conj_plus!(
        CallFresh {
            s: Box::new(Eq_ { t: Object(7) }),
        },
        CallFresh {
            s: Box::new(disj_plus!(Eq_ { t: Object(5) }, Eq_ { t: Object(6) }))
        }
    );

    for s in conj.from_state(s.clone(), c.clone()) {
        println!("trd: {s:?}");
    }

    let fives = Fives { x: Object(5) };
    let sixes = Fives { x: Object(6) };
    let disj = CallFresh {
        s: Box::new(Disj {
            g1: Rc::new(Box::new(fives)),
            g2: Rc::new(Box::new(sixes)),
        }),
    };
    let mut breaking = 0;
    for s in disj.from_state(s.clone(), c.clone()) {
        println!("frt {s:?}");
        let size = s.0.len();
        let count = s.1;
        println!("length: {size} count: {count}");
        println!("=======================================================");
        breaking += 1;
        if breaking > 3 {
            break;
        }
    }

    // well these are
    let noanswer = CallFresh {
        s: Box::new(Conj {
            g1: Rc::new(Box::new(Eq_ { t: Object(3) })),
            g2: Rc::new(Box::new(Eq_ { t: Object(4) })),
        }),
    };

    for s in noanswer.from_state(s.clone(), c.clone()) {
        println!("dunno {s:?}")
    }

    let conde = conde!(
        (Eq_ { t: Object(3) }, Eq_ { t: Object(3) }),
        (Eq_ { t: Object(4) }, Eq_ { t: Object(4) })
    );

    //TODO hmm have to keep the count (x&y's in your head........, maybe macro x,y etc. to correct var count in macro? To not have closures.)
    let fresh = fresh!((1, 2), Eq_ { t: Var(1) }, Eq_ { t: Object(3) });

    for s in fresh.from_state(s.clone(), c.clone()) {
        println!("macros1 {s:?}");
    }

    //Hmmmmmmmmmmmmm now can do (== q 'pea) but not (== 'pea 'pea)
    let ehm = run_maybe_star(Box::new(fresh!((1), Eq_ { t: Object("pea") })));
    println!("book_cell14: {ehm:?}");

    //Hmmmmmmmmmmmmm now can do (== q p), we get one unbound var
    let ehm: Vec<Term<String>> = run_maybe_star(Box::new(fresh!((1), Eq_ { t: Var(2) })));
    println!("unbound...: {ehm:?}");

    //Var 1 remains unbound
    let ehm = run_maybe_star(Box::new(fresh!((1,2), Eq_ { t: Object("pea") })));
    println!("book_cell24: {ehm:?}");
}

enum Reified<T>
where
    T: Debug + Hash + Eq + Clone,
{
    Value(T),
    Variable(T),
}

fn mk_reify<T>(
    scs: Box<dyn Iterator<Item = (Substitution<T>, i32)>>,
) -> Map<Box<dyn Iterator<Item = (Substitution<T>, i32)>>, fn((Substitution<T>, i32)) -> Term<T>>
where
    T: Debug + Hash + Eq + Clone,
{
    scs.map(reify_state)
}

fn reify_state<T>((s, _c): (Substitution<T>, i32)) -> Term<T>
where
    T: Debug + Hash + Eq + Clone,
{
    let fst_var: Term<T> = Var(1);
    walk(&fst_var, s)
}

//TODO the macros
fn run_maybe<T>(n: usize, fresh_body: Box<dyn Stream<T>>) -> Vec<Term<T>>
where
    T: Debug + Hash + Eq + Clone,
{
    fresh_body.from_state(HashMap::new(), 0)
        .map(reify_state)
        .take(n)
        .collect()
}

fn run_maybe_star<T>(fresh_body: Box<dyn Stream<T>>) -> Vec<Term<T>>
where
    T: Debug + Hash + Eq + Clone,
{
    fresh_body.from_state(HashMap::new(), 0)
        .map(reify_state)
        .collect()
}

#[macro_export]
macro_rules! conj_plus {
    ($head:expr) => ($head);
    ($head:expr $(, $tail:expr)*) => (Conj { g1: Rc::new(Box::new($head)), g2: Rc::new(Box::new(conj_plus!($($tail),*)))});
}

#[macro_export]
macro_rules! disj_plus {
    ($head:expr) => ($head);
    ($head:expr $(, $tail:expr)*) => (Disj { g1: Rc::new(Box::new($head)), g2: Rc::new(Box::new(disj_plus!($($tail),*)))});
}

#[macro_export]
macro_rules! conde {
    ($(($head:expr $(, $tail:expr)*)),*) => {
                disj_plus!( $(
                conj_plus!($head $(, $tail)*)
            ),*)
    }
}

#[macro_export]
macro_rules! fresh {
//     fresh(<vars>) (<stmts>)
    // can do a loop fresh!(amount, (<stmts>,))
    (($v:expr), $head:expr $(, $tail:expr)*) => {
        CallFresh {s: Box::new(conj_plus!($head $(, $tail)*))}
    };
    (($v:expr $(, $vs:expr)*), $head:expr $(, $tail:expr)*) => {
        CallFresh {s: Box::new(fresh!(($($vs:expr),*), $head $(, $tail)*))}
    }
}

//Impl stuff.......
struct Fives<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    x: Term<T>,
}

impl<T> Stream<T> for Fives<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Box<dyn Iterator<Item = (Substitution<T>, i32)>> {
        Box::new(FivesIterator::new(self.x.clone(), s, c))
    }
}

struct FivesIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    x: Term<T>,
    s: Substitution<T>,
    c: i32,
}

impl<T> FivesIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn new(x: Term<T>, s: Substitution<T>, c: i32) -> Self {
        Self { x, c, s }
    }
}

impl<T> Iterator for FivesIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    type Item = (Substitution<T>, i32);

    fn next(&mut self) -> Option<Self::Item> {
        let eq = Eq_ { t: self.x.clone() };
        eq.from_state(self.s.clone(), self.c.clone()).next()
    }
}
