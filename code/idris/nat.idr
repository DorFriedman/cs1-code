module nat

import bool
import pair

-- NOTE: Original post edited to remove %default total. You *will* get warnings about totality. Proceed anyway.

{- We now introduce a type to represent the natural numbers using
Peano arithmetic.

In this arithmetic, there are two especially important rules
- zero is a natural number
- the successor of any natural number is a natural number

From this pair of rules arises an infinity of natural numbers. Zero is
a natural number, and the successor of any natural number is a natural
number, so the successor of zero is a natural number; and now, because
the successor of zero is a natural number, its successor, i.e., the
successor of (the successor of zero) is one too; ad infinitum!

HOMEWORK #1: Learn this way of thinking by rehearsing it mentally
until you've really got it. Work with friends. Explain it to each
other. Test each other. -}

{- We capture this idea with the following deceptively simple data
type definition. What it says is that nat is a type, and there are two
ways to construct a nat. One is to use the constructor, O. The other
is to apply the constructor S to a value that is already known to be
of type nat.  Because O is already known to be of type nat, you can
thus apply S to O (yielding the term (S O)) and the result is also of
type nat. And now because it is a value of type nat, you can apply S
to it to get another value of type nat. Ad infinitum.

HOMEWORK #2: Study and think about this example as many times as
necessary to get it. Work with friends. Teach each other. Test each
other.  -}

-- Here's our data type definition.

||| A data type to represent the natural numbers.  We use a lower case
||| "n" in the name to distinguish our nat type from Idris's Nat type
data nat = O | S nat

-- HOMEWORK #3: In the REPL ask what are the types of nat, O and S.
-- Be sure you understand the answers! Teach, learning from and test
-- friends.

{-
Here are some examples of some objects (named values) of type nat.
-}

-- zero
z: nat
z = O

-- one
o: nat
o = S O

-- two
t: nat
t = S (S O)

-- three
r: nat
r = S t

-- HOMEWORK #5: Make up some examples of your own. Add them below this
-- comment.

-- four
p: nat
p = S r

-- five
q: nat
q = S p

-- HOMEWORK #6: In the REPL, evaluate the types and values of z, o, t
-- and r, as well as your own values. (E.g., ":t o" will report the
-- type of o, and "o" will report its value (along with its type). Be
-- sure you understand the answers. In particular be sure you
-- understand how the variable expression, "r" is evaluated! Yup: r is
-- unfolded to S t; t is unfolded to S (S O); so the whole expression
-- unfolds to S (S (S O)). Be able to explain this evaluation process
-- verbally and in writing. Teach and test each other.

--This is simply like writing variables within mathematical equations:
--r is a variable that represents a term and/or value. In this case, r
--represents S t. t, now, is another variable; so, you can take what t
--represents and replace it with t, and on and on. Thus, you can get
--the full unfolding that comes out to be S (S (S O)).

{-
The rest of this file contains functions operating on nats.
-}

||| returns true if the argument is (represents) zero, otherwise false
isZero: nat -> bool
isZero O = true
isZero _ = false

-- HOMEWORK #7 Bind x to the result of applying isZero to r. Add your
-- code below this comment.

x: bool
x = isZero r

||| returns the successor of any given nat
succ: nat -> nat
succ n = S n

-- HOMEWORK #8 Bind y to the result of applying succ to O. Add code here.

y: nat
y = succ O

||| returns the predecessor of any given nat
pred: nat -> nat
pred O = O
pred (S n) = n

{- Next we define a function that when given a natural number returns
true if it's even and false otherwise. It is very, very important to
understand this example, your first example of a recursive function -}

||| return true if the argument is even otherwise false
evenb: nat -> bool
evenb O = true
evenb (S O) = false
evenb (S (S n)) = evenb n

||| return false if the argument is even otherwise true
||| i.e., return true if it's odd and false if it's even
oddb: nat -> bool
oddb n = not (evenb n)

-- HOMEWORK #9 Bind a, b, c, d, e respectively to the results of
-- applying evenb to O, (S O), (S (S O)) r, and (S r),
-- respectively. Add your code here.

a: bool
a = evenb O

b: bool 
b = evenb (S O)

c: bool
c = evenb (S (S O))

d: bool
d = evenb r

e: bool
e = evenb (S r)

--HOMEWORK #10 In English, describe all of the steps involved in
--evaluating the function application expression, evenb (S r). You
--must be able to explain this process to yourself and your friends!
--The idea is that you keep on applying reduction (simplification)
--rules until you get to a value that cannot be reduced further. Write
--your answer by filling in the following block comment.

{- evenb (S r) would yield you bool value of "true," because (S r) is
the same thing as (S (S (S (S O)))). And, if O is an even number (as
defined in the given set of lines of codes previously), then the
consecutive-consecutive-consecutive-consecutive natural number after O
would be an even one. -}

-- HOMEWORK #11 Here's a function that takes a pair of natural numbers
-- (values of type nat) and that returns their sum (as another value
-- of type nat). We give you the function name and type along with the
-- left hand sides of the two rules that you'll need. We've filled in
-- the right hand sides with placeholders. Your task is to replace the
-- right hand sides of the two rules.

-- Here are a few examples of how this function should work, to get
-- you going. We use ==> to mean "reduces to" or "returns"

-- addp (mkPair O O) ==> O
-- addp (mkPair O (S (S O))) ==> S (S O)
-- addp (mkPair (S O) O) ==> S O
-- addp (mkPair (S (S O)) (S (S (S O)))) ==> S (S (S (S (S O))))

-- Completing the first rule should be pretty easy. Just think about
-- the most basic properties of arithmetic. The second rule is more
-- challenging. Go back and study the evenb example. Notice how we
-- used evenb within its own definition! The idea is that we can
-- compute evenb for a big number by returning the result that we get
-- when we apply it to a number that is two less than the number that
-- we started with. You're going to use a similar idea here.

-- This is the hardest but also the most important problem in this
  -- problem set. There is *no grading penalty* for not getting an
  -- answer to this problem. There is no grading reward for getting
  -- it. Really: do *not* collaborate on this problem. It will help
  -- us instructors to see how you do.

||| given a pair of natural numbers, return its sum
addp: pair nat nat -> nat
addp (mkPair O m) = m
addp (mkPair (S n) m) = S (addp (mkPair n m))

-- HOMEWORK 12 -- Use the REPL to verify that these tests return the
-- expected results

||| a test, expecting O
h1: nat
h1 = addp (mkPair O O)

||| a test, expecting S (S O)
h2: nat
h2 = addp (mkPair O (S (S O)))

||| a test, expecting (S O)
h3: nat
h3 = addp (mkPair (S O) O)

||| a test, expecting (S (S (S (S (S O)))))
h4: nat
h4 = addp (mkPair (S (S O)) (S (S (S O))))

|||given a pair of natural numbers, return its sum
-- multp: pair nat nat -> nat
-- multp (mkPair O m) = ?multp1
-- multp (mkPair (S n) m) = ?multp2

|||given a pair of natural numbers, return its sum
multp: pair nat nat -> nat
multp (mkPair O m) = O
multp (mkPair (S n) m) = addp (mkPair m (multp (mkPair n m)))

factp: nat -> nat
factp O = (S O)
factp (S n') = multp (mkPair (S n') (factp n'))

||| given a pair of natural numbers, (a, b), return a-b.
|||If a <= b, the answer will be O.
subp: pair nat nat -> nat
subp (mkPair O m) = O
subp (mkPair n O) = n
subp (mkPair (S n) (S m)) = subp (mkPair n m)

||| given a pair of natural numbers, (a, b), return true
||| if a <= b otherwise return false
lep: pair nat nat -> bool
lep (mkPair O m) = true
lep (mkPair (S n) O) = false
lep (mkPair (S n) (S m)) = lep (mkPair n m)
