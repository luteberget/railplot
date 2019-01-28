// transitive reduction

use std::collections::HashSet;
use std::collections::HashMap;
use std::hash::Hash;

pub fn trans_red<T: Eq+Hash+Copy+Clone>(set :&mut HashSet<(T,T)>) {
    let map = {
        let mut map = HashMap::new();
        for (a,b) in set.iter() {
            map.entry(*a).or_insert_with(Vec::new).push(*b);
        }
        map
    };

    let orig = set.iter().cloned().collect::<Vec<_>>();
    for (a,b) in orig {
        if !set.contains(&(a,b)) { continue; }
        let mut stack = vec![b];
        while stack.len() > 0 {
            let b = stack.pop().unwrap();
            for c in map.get(&b).unwrap_or(&Vec::new()) {
                stack.push(*c);
                set.remove(&(a,*c));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn red() {
        use std::collections::HashSet;
        let t = vec![(1,2),(2,3),(3,4),(1,3),(1,4)];
        let mut set :HashSet<(usize,usize)> = t.into_iter().collect();
        println!("before {:?}", set);
        super::trans_red(&mut set);
        println!("after  {:?}", set);

        let out = vec![(1,2),(2,3),(3,4)];
        assert_eq!(set, out.into_iter().collect());
    }
}
