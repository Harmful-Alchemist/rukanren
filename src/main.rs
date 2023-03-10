// From http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

// use crate::Term::{Object, Pair, Var};
use crate::Term::{Object, Var};
use std::cell::RefCell;
use std::cmp::Eq;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::rc::Rc;
use std::vec;

#[derive(PartialEq, Eq, Hash, Clone)]
enum Term<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    Var(i32),
    // Pair(Rc<RefCell<Term<T>>>, Rc<RefCell<Term<T>>>),
    Object(T),
}

impl<T> Debug for Term<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Var(n) => write!(f, "_{n}"),
            Object(o) => o.fmt(f),
            // Pair(t1,t2) => write!(f, "({t1:?},{t2:?})")
        }
    }
}

trait Stream<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>>;
}

fn walk<T: Eq>(u: &Term<T>, s: Substitution<T>) -> Term<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    let mut pr = u;
    while s.contains_key(pr) {
        let pr_ = s.get(pr).unwrap();
        if pr == pr_ {
            break;
        }
        pr = pr_;
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
    u: Term<T>,
    v: Term<T>,
}

impl<T> Stream<T> for Eq_<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>> {
        Rc::new(RefCell::new(EqIterator {
            u: self.u.clone(),
            v: self.v.clone(),
            s,
            c: c.clone(),
            done: false,
        }))
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
    s: Rc<RefCell<dyn Stream<T>>>,
}

impl<T> Stream<T> for CallFresh<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        mut s: Substitution<T>,
        c: i32,
    ) -> Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>> {
        ext_s(Var(c), Var(c), &mut s);
        self.s.borrow().from_state(s, c + 1)
    }
}

struct Disj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    g1: Rc<RefCell<dyn Stream<T>>>,
    g2: Rc<RefCell<dyn Stream<T>>>,
}

impl<T> Stream<T> for Disj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>> {
        let x = self.g1.borrow().from_state(s.clone(), c.clone());
        let y = self.g2.borrow().from_state(s.clone(), c.clone());
        Rc::new(RefCell::new(DisjIterator { m: 0, g1: x, g2: y }))
    }
}

struct DisjIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    m: usize,
    g1: Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>>,
    g2: Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>>,
}

impl<T> Iterator for DisjIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    type Item = (Substitution<T>, i32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.m % 2 == 0 {
            // println!("1st");
            let opt = self.g1.borrow_mut().next();
            if !opt.is_none() {
                self.m = self.m + 1;
                return opt;
            }
        }
        self.m = self.m + 1;
        // println!("2nd");
        self.g2.borrow_mut().next()
    }
}

struct Conj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    g1: Rc<RefCell<dyn Stream<T>>>,
    g2: Rc<RefCell<dyn Stream<T>>>,
}

impl<T> Stream<T> for Conj<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    fn from_state(
        &self,
        s: Substitution<T>,
        c: i32,
    ) -> Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>> {
        let x = self.g1.borrow().from_state(s.clone(), c.clone());
        Rc::new(RefCell::new(ConjIterator {
            s: x,
            g: self.g2.clone(),
            next: None,
        }))
    }
}

struct ConjIterator<T>
where
    T: Debug + Hash + Eq + Clone + 'static,
{
    s: Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>>,
    g: Rc<RefCell<dyn Stream<T>>>,
    next: Option<Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>>>,
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
            if let Some((s_, c_)) = self.s.borrow_mut().next() {
                self.next = Some(self.g.borrow().from_state(s_.clone(), c_.clone()));
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
        //     if unify(&car_u.borrow(), &car_v.borrow(), s) {
        //         unify(&cdr_u.borrow(), &cdr_v.borrow(), s)
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
        s: Rc::new(RefCell::new(Disj {
            g1: Rc::new(RefCell::new(Eq_ {
                v: Var(0),
                u: Object(5),
            })),
            g2: Rc::new(RefCell::new(Eq_ {
                v: Var(0),
                u: Object(6),
            })),
        })),
    };

    for s in &mut *disj.from_state(s.clone(), c.clone()).borrow_mut() {
        println!("snd: {s:?}")
    }

    Rc::new(RefCell::new(CallFresh {
        s: Rc::new(RefCell::new(Eq_ {
            v: Var(0),
            u: Object(7),
        })),
    }));

    let conj = conj_plus!(
        CallFresh {
            s: Rc::new(RefCell::new(Eq_ {
                u: Var(0),
                v: Object(7)
            })),
        },
        CallFresh {
            s: Rc::new(RefCell::new(disj_plus!(
                Eq_ {
                    u: Var(1),
                    v: Object(5)
                },
                Eq_ {
                    u: Var(1),
                    v: Object(6)
                }
            )))
        }
    );

    for s in &mut *conj.from_state(s.clone(), c.clone()).borrow_mut() {
        println!("trd: {s:?}");
    }

    let fives = Fives { x: Object(5) };
    let sixes = Fives { x: Object(6) };
    let disj = CallFresh {
        s: Rc::new(RefCell::new(Disj {
            g1: Rc::new(RefCell::new(fives)),
            g2: Rc::new(RefCell::new(sixes)),
        })),
    };
    let mut breaking = 0;
    for s in &mut *disj.from_state(s.clone(), c.clone()).borrow_mut() {
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
        s: Rc::new(RefCell::new(Conj {
            g1: Rc::new(RefCell::new(Eq_ {
                u: Var(0),
                v: Object(3),
            })),
            g2: Rc::new(RefCell::new(Eq_ {
                u: Var(0),
                v: Object(4),
            })),
        })),
    };

    for s in &mut *noanswer.from_state(s.clone(), c.clone()).borrow_mut() {
        println!("dunno {s:?}")
    }

    let conde = || {
        conde!(
            (
                Eq_ {
                    u: Var(0),
                    v: Object(3)
                },
                Eq_ {
                    u: Var(0),
                    v: Object(3)
                }
            ),
            (
                Eq_ {
                    u: Var(0),
                    v: Object(4)
                },
                Eq_ {
                    u: Var(0),
                    v: Object(4)
                }
            )
        )
    };

    let condes = run_maybe_star(Rc::new(RefCell::new(conde())));
    println!("conde* {condes:?}");

    let conde1 = run_maybe(1, Rc::new(RefCell::new(conde())));
    println!("conde1 {conde1:?}");

    //TODO still need to keep count yourself...... But could use replacing in the macro x=0 y=1 etc depending on 1st list.
    // now need to keep track manually... Difficult when nesting.
    let fresh = fresh!(
        (0, 1),
        Eq_ {
            u: Var(0),
            v: Var(1)
        },
        Eq_ {
            u: Var(0),
            v: Object(3)
        }
    );

    for s in &mut *fresh.from_state(s.clone(), c.clone()).borrow_mut() {
        println!("macros1 {s:?}");
    }

    let fm = run_maybe_star(Rc::new(RefCell::new(fresh)));

    println!("run macros {fm:?}");

    let ehm = run_maybe_star(Rc::new(RefCell::new(fresh!(
        (0),
        disj_plus!(
            Eq_ {
                u: Var(0),
                v: Object("pea")
            },
            Eq_ {
                u: Var(0),
                v: Object("pod")
            }
        )
    ))));
    println!("book_cell_1.14_inspired: {ehm:?}");

    let ehm = run_maybe_star(Rc::new(RefCell::new(fresh!(
        (0),
        Eq_ {
            u: Var(10),
            v: Object("pea")
        }
    ))));
    println!("unbound...: {ehm:?}");

    let ehm = run_maybe_star(Rc::new(RefCell::new(fresh!(
        (0, 1),
        Eq_ {
            u: Var(1),
            v: Object("pea")
        }
    ))));
    println!("book_cell_1.24: {ehm:?}");
}

fn reify_state<T>((s, _c): (Substitution<T>, i32)) -> Vec<Term<T>>
where
    T: Debug + Hash + Eq + Clone,
{
    let fst_var: Term<T> = Var(0);
    match walk(&fst_var, s.clone()) {
        // Pair(x, y) => {let mut a = walk_star(x, s.clone()); let mut b = walk_star(y,s); a.append(&mut b); a },
        x => {
            vec![x]
        }
    }
}

fn walk_star<T>(x: Rc<RefCell<Term<T>>>, s: Substitution<T>) -> Vec<Term<T>>
where
    T: Debug + Hash + Eq + Clone,
{
    match walk(&x.borrow(), s.clone()) {
        // Pair(x, y) => {let mut a = walk_star(x, s.clone()); let mut b = walk_star(y,s); a.append(&mut b); a },
        x => {
            vec![x]
        }
    }
}

//TODO the macros? Maybe... Take car tho now requiring manual call to fresh, want to move here but need to be clearer with fresh too.
fn run_maybe<T>(n: usize, fresh_body: Rc<RefCell<dyn Stream<T>>>) -> Vec<Term<T>>
where
    T: Debug + Hash + Eq + Clone,
{
    let it = fresh_body.borrow().from_state(HashMap::new(), 0);
    let mut ret = Vec::new();
    let fst_var: Term<T> = Var(0);
    for (s, _c) in (&mut *it.borrow_mut()).take(n) {
        match walk(&fst_var, s.clone()) {
            // Pair(x, y) => {let mut a = walk_star(x, s.clone()); let mut b = walk_star(y,s); a.append(&mut b); a },
            x => ret.append(&mut vec![x]),
        }
    }
    ret
}

fn run_maybe_star<T>(fresh_body: Rc<RefCell<dyn Stream<T>>>) -> Vec<Term<T>>
where
    T: Debug + Hash + Eq + Clone,
{
    let it = fresh_body.borrow().from_state(HashMap::new(), 0);
    let mut ret = Vec::new();
    let fst_var: Term<T> = Var(0);
    for (s, _c) in &mut *it.borrow_mut() {
        match walk(&fst_var, s.clone()) {
            // Pair(x, y) => {let mut a = walk_star(x, s.clone()); let mut b = walk_star(y,s); a.append(&mut b); a },
            x => ret.append(&mut vec![x]),
        }
    }
    ret
}

#[macro_export]
macro_rules! conj_plus {
    ($head:expr) => ($head);
    ($head:expr $(, $tail:expr)*) => (Conj { g1: Rc::new(RefCell::new($head)), g2: Rc::new(RefCell::new(conj_plus!($($tail),*)))});
}

#[macro_export]
macro_rules! disj_plus {
    ($head:expr) => ($head);
    ($head:expr $(, $tail:expr)*) => (Disj { g1: Rc::new(RefCell::new($head)), g2: Rc::new(RefCell::new(disj_plus!($($tail),*)))});
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
    (($v:expr), $head:expr $(, $tail:expr)*) => {
        CallFresh {s: Rc::new(RefCell::new(conj_plus!($head $(, $tail)*)))}
    };
    (($v:expr $(, $vs:expr)*), $head:expr $(, $tail:expr)*) => {
        CallFresh {s: Rc::new(RefCell::new(fresh!(($($vs),*), $head $(, $tail)*)))}
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
    ) -> Rc<RefCell<dyn Iterator<Item = (Substitution<T>, i32)>>> {
        Rc::new(RefCell::new(FivesIterator::new(self.x.clone(), s, c)))
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
        let eq = Eq_ {
            v: Var(0),
            u: self.x.clone(),
        };
        eq.from_state(self.s.clone(), self.c.clone())
            .borrow_mut()
            .next()
    }
}
