use std::str::ParseBoolError;

/// Check that two vecs have same elements regardless of order
pub fn match_array<
    T: Ord + Clone + PartialEq<S> + std::fmt::Debug,
    S: Ord + Clone + std::fmt::Debug,
>(
    vec1: Vec<T>,
    vec2: Vec<S>,
) -> bool {
    let mut s1 = vec1.clone();
    let mut s2 = vec2.clone();
    s1.sort();
    s2.sort();
    if s1 != s2 {
        println!("left:  {:?}", s1);
        println!("right: {:?}", s2);
    }
    s1 == s2
}
