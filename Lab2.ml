(* Helper functions for rational numbers *)
let num = fst
let den = snd

let rec gcd i j =
    if i <> 0
        then if j > i
            then gcd i (j - i)
            else gcd (i - j) j
    else j

(* Create a rational number *)
let rat n d = 
    let z = gcd n d in
    (n/z, d/z)

(* Rational number operations *)
let ratAdd a b = 
    let newnum = (num a * den b) + (den a * num b) in
    let newden = (den a * den b) in
    rat newnum newden

let ratMul a b =
    let newnum = (num a * num b) in
    let newden = (den a * den b) in
    rat newnum newden

let ratDiv a b = 
    let newnum = (num a * den b) in
    let newden = (den a * num a) in
    rat newnum newden

let ratGt a b =
    (num a * den b) > (num b * den a)

(* Compute the value of Euler's number *)
let euler () =
    let eps = rat 1 10000 in
    let rec calc c s t =
        if ratGt t eps then
            let new_s = ratAdd s t in
            let new_t = ratDiv t c in
            let new_c = ratAdd c (rat 1 1) in
            calc new_c new_s new_t
        else
            s
    in
    calc (rat 0 1) (rat 0 1) (rat 1 1)
