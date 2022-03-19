
-- CHAPTER 1 - All You Need is Lambda

-- Even the greatest -- mathematicians, the ones that we would put into our mythology of great mathematicians, 
-- had to do a great deal of leg work in order to get to the solution in the end.
-- Daniel Tammett

-- 1.1 All You Need is Lambda

-- a very brief introduction to the lambda calculus, a model of computation devised in the 1930s by Alonzo Church. A calculus is a method of calculation or reasoning;
-- the lambda calculus is one process for formalizing a method. Like Turing machines, the lambda calculus formalizes the concept of effective computability, thus determining
-- which problems, or classes of problems, can be solved.


-- Lambda calculus is your foundation, because Haskell is a lambda calculus.

-- 1.2 What is functional programming?
-- Functional programming is a computer programming paradigm that relies on functions modeled on mathematical functions.
-- The essence of functional programming is that programs are a combination of expressions. Expressions include concrete values, variables, and also functions. 
-- Functions have a more specific definition: they are expressions that are applied to an argument or input and, once applied, can be reduced or evaluated. 
-- In Haskell, and in functional programming more generally, functions are first-class: 
-- they can be used as values or passed as arguments, or inputs, to yet more functions. 
-- *********************************************** Expressions, Values, variables, Functions ******************************************   
-- *********************************************** Functions are First Class  ************************************************************


-- Functional programming languages are all based on the lambda calculus. 
-- Some languages in this general category incorporate features that aren’t translatable into lambda expressions.
-- Haskell is a pure functional language, because it does not. We’ll further address this notion of purity later in
-- the book, but it isn’t a judgment of the moral worth of other languages.
-- ********************************************** Pure Functional Language *********************************************************

-- The word purity in functional programming is sometimes also used to mean what is more properly called referential transparency. 
-- Referential transparency means that the same function, given the same values to evaluate, will always return the same result in pure functional programming, 
-- as they do in math.
-- ********************************************** referential transparency *********************************************************



-- Haskell’s pure functional basis also lends it a high degree of abstraction and composability. Abstraction allows you to
-- write shorter, more concise programs by factoring common, repeated structures into more generic code that can be reused.
-- Haskell programs are built from separate, independent functions, kind of like LEGO : the functions are bricks that can be assembled and reassembled.

-- ********************************************** abstraction & composability *********************************************************

-- https://stackoverflow.com/questions/2887013/what-does-composability-mean-in-context-of-functional-programming

-- Composability allows for the developer community to continually raise the level of abstraction, multiple levels, without being chained to the base layer.

-- The point is that functional programming is well suited to this: 
-- With immutable variables and restrictions on side effects you can compose more easily as you need not worry about what happens under the hood in the function being called 
--    - like updating a shared variable so the result will be invalid for certain sequences of operations, 
--    or accessing a shared lock so certain call sequences will give a deadlock.

-- Composition in computer science is the ability to assemble complex behaviour by aggregating simpler behaviour. 
-- Functional decomposition is an example of this, whereby a complex function is broken up into smaller easy-to-grasp functions and assembled into the final system 
-- by a top-level function. The top-level function can be said to have "composed" the pieces into the whole.

-- I agree with Marcelo Cantos's answer, but I think it may assume more background than some readers have, 
-- which is also related to why composition in functional programming is special. 
-- Functional programming function composition is essentially identical to function composition in mathematics. 
-- In math, you may have a function f(x) = x^2, and a function g(x) = x + 1. Composing the functions means creating a new function, 
-- in which the function arguments are given to the "inner" function, and the "inner" function's output serves as input to the "outer" function. 
-- Composing f outer with g inner could be written f(g(x)). If you provide a value of 1 for x, then g(1) == 1 + 1 == 2, so f(g(1)) == f(2) == 2^2 == 4. 
-- More generally, f(g(x)) == f(x + 1) == (x+1)^2. I described composition using the f(g(x)) syntax, but mathematicians often prefer a different syntax, (f . g)(x). 
-- I think this is because it makes clearer that f composed with g is a function in its own right, which takes a single argument.


-- 1.3 What is a function?
-- ********************************************** Function , inputs, outputs *********************************************************
-- A function is a relation between a set of possible inputs and a set of possible outputs.


-- function itself defines and represents that relationship. When you apply a function such as addition to two inputs, 
-- it maps those two inputs to an output—the sum of those numbers.


-- first value is the input and the second is the output:
-- 𝑓(1) = 𝐴
-- 𝑓(2) = 𝐵
-- 𝑓(3) = 𝐶
-- input set is {1, 2, 3}, and the output set is {A, B, C}.
-- our hypothetical function will always return the value A given the input 1—no exceptions!

-- is not a valid function:
-- 𝑓(1) = 𝑋
-- 𝑓(1) = 𝑌
-- 𝑓(2) = 𝑍

-- the notion of referential transparency we mentioned earlier: given the same input, the output should be predictable.

-- 𝑓(1) = 𝐴
-- 𝑓(2) = 𝐴
-- 𝑓(3) = 𝐴
-- Yes, having the same output for more than one input is
-- valid. 

-- ********************************************** domain co-domain image pg 4/5 *********************************************************

-- What matters here is that the relationship of inputs and outputs is defined by the function and that the output is predictable
-- when you know the input and the function definition.



-- This function is again named 𝑓:
-- 𝑓(𝑥) = 𝑥 + 1
-- This function takes one argument, which we have named 𝑥. The relationship between the input, 𝑥, and the output is described in the function body. 
-- It will add 1 to whatever value 𝑥 is and return that result.


-- ********************************************** apply this function to a value *********************************************************


-- apply this function to a value, such as 1, we substitute the value for 𝑥:
-- 𝑓(1) = 1 + 1
-- 𝑓 applied to 1 equals 1 + 1. That tells us how to map the input to an output: 1 added to 1 becomes 2:
-- 𝑓(1) = 2


-- Understanding functions in this way—as a mapping of a set of inputs to a set of outputs—is crucial to understanding functional programming.




-- 1.4 The structure of lambda expressions 

-- ********************************************** Lambda calculus 3 basic components *********************************************************
-- ********************************************** expressions, variables, and abstractions *********************************************************
-- ********************************************** expressions - superset of all those things *********************************************************

-- The lambda calculus has three basic components, or lambda terms: expressions, variables, and abstractions. The word expression
-- refers to a superset of all those things: an expression can be a variable name, an abstraction, or a combination of those things. 
-- The simplest expression is a single variable. Variables here have no meaning or value; they are only names for potential inputs to functions.

-- ********************************************** Abstraction - head body argument  *********************************************************
-- An abstraction is a function. lambda term that has a head (a lambda) and a body and is applied to an argument. 
-- An argument is an input value

-- Abstractions consist of two parts: the head and the body. The head of the function is a / (lambda) followed by a variable name. 
-- The body of the function is another expression. So, a simple function might look like this:
-- /x.x


-- The variable named in the head is the parameter and binds all instances of that same variable in the body of the func tion.
-- That means, when we apply this function to an argument, each 𝑥 in the body of the function will have the value of that argument.

-- ********************************************** Application *********************************************************
-- The act of applying a lambda function to an argument is called application, and application is the lynchpin of the lambda calculus.


-- ********************************************** Named Function f    vs   Anonymous function Lambda  *********************************************************
-- In the previous section, we were talking about functions called 𝑓, but the lambda abstraction /x.x has no name. It is an anonymous function. 
-- A named function can be called by name by another function; an anonymous function cannot.

-- Let’s break down the basic structure:

--             /x.                          x
--            **-**      
--     extent if the HEAD  /x.            BODY

--     the x in /x. part 
--             the single PARAMETER of the function. This binds any variables with the same name in the body of the function.

--     The x after head   /x.     is the body. 

-- The dot (.) separates the parameters of the lambda from the function body.

-- ********************************************** Head Parameter Body   *********************************************************

-- The abstraction as a whole has no name, but the reason we call it an abstraction is that it is a generalization, or abstraction,
-- from a concrete instance of a problem, and it abstracts through the introduction of names. 

-- The names stand for particular values, but by using named variables, we allow for the possibility
-- of applying the general function to different values (or, perhaps even values of different types, as we’ll see later). 
-- When we apply the abstraction to arguments, we replace the names with values, making it concrete.

-- Alpha equivalence

-- ********************************************** Alpha equivalence  *********************************************************
-- The variable 𝑥 here is not semantically meaningful except in its role in that single expression. Because of this, there’s a form
-- of equivalence between lambda terms called alpha equivalence.
-- This is a way of saying that the following expressions all mean the same thing:

-- /x.x
-- /d.d
-- /z.z

-- In principle, they’re all the same function.



-- 1.5 Beta reduction
-- ********************************************** Beta reduction  *********************************************************

-- When we apply a function to an argument, we substitute the input expression for all instances of bound variables within
-- the body of the abstraction. You also eliminate the head of the abstraction, since its only purpose is to bind a variable. This
-- process is called beta reduction.
-- Let’s use the function we had above:

-- /x.x

-- We’ll do our first beta reduction using a number
-- We apply the function above to 2, substitute 2 for each bound variable in the body of the function, and eliminate the head:

-- /x.x 2
-- 2

-- ********************************************** identity function  *********************************************************
-- The only bound variable is the single 𝑥, so applying this function to 2 returns 2.
-- This function is the identity function. All it does is accept a single argument 𝑥 and return that same argument.

-- Note that this is the same as the identity function in mathematical notation: 𝑓(𝑥) = 𝑥.
-- One difference is that 𝑓(𝑥) = 𝑥 is a declaration involving a function named 𝑓, while the above lambda abstraction is a function.

-- all instances of 𝑥 within the function body must have the same value.




-- Beta reduction is this process of applying a lambda term to an argument, replacing the bound variables with the value of
-- the argument, and eliminating the head. Eliminating the head tells you the function has been applied.
-- We can also apply our identity function to another lambda abstraction:

-- (/x.x)(/y.y)

-- We’ll use a new syntax here, [𝑥 ∶= 𝑧], to indicate that 𝑧 will be substituted for all occurrences of 𝑥 (here 𝑧 is the function /y.y
-- sample exercise pg 12.
-- ********************************************** [𝑥 ∶= 𝑧] syntax   *********************************************************

-- Applications in the lambda calculus are left associative. Unless specific parentheses suggest otherwise, they associate, or group, to the left. 
-- ********************************************** Applications are Left Associative   *********************************************************
-- So, this:
-- (\x.x)(\y.y) z
-- Can be rewritten as:
-- ((\x.x)(\y.y)) z

-- We’ll look at functions below that have multiple heads and also free variables (that is, variables in the body that are not bound by the head),

-- ********************************************** Free variables / Bound variable / Irreducible  *********************************************************

-- Beta reduction stops when there are no longer unevaluated functions applied to arguments.


-- Free variables
-- The purpose of the head of the function is to tell us which variables to replace when we apply our function, that is, to bind the variables.
-- A bound variable must have the same value throughout the expression.
-- But sometimes, the body expression has variables that are not named in the head. We call those variables free variables.
-- In the following expression:

-- /x.xy
-- The 𝑥 in the body is a bound variable, because it is named in the head of the function, while the 𝑦 is a free variable, because it is not. 
-- When we apply this function to an argument, nothing can be done with the 𝑦. It remains irreducible.


-- ********************************************** alpha equivalence does not apply to free variables  *********************************************************

-- Note that alpha equivalence does not apply to free variables.
-- That is,  /𝑥.𝑥𝑧 and /𝑥.𝑥𝑦 are not equivalent, because 𝑧 and 𝑦 might be different things.

-- However, /𝑥𝑦.𝑦𝑥 and /𝑎𝑏.𝑏𝑎 are equivalent due to alpha equivalence, as are /𝑥.𝑥𝑧 and /𝑦.𝑦𝑧, because the free variable is left alone.


-- ********************************************** Multiple arguments / Currying *********************************************************
-- 1.6 Multiple arguments

-- Each lambda can only bind one parameter and can only accept one argument. Functions that require multiple arguments have multiple, nested heads. 
-- When you apply it once and eliminate the first (leftmost) head, the next one is applied and so on. 
-- later rediscovered and named after Haskell Curry and is commonly called currying.


-- /xy.xy
-- Is a convenient shorthand for two nested lambdas (one for each argument, 𝑥 and 𝑦):
-- /x(/y.xy)

-- When you apply the first argument, you’re binding 𝑥, eliminating the outer lambda, and have /𝑦.𝑥𝑦 with 𝑥 being whatever the outer lambda was bound to.
-- examples pg 15-16-17 etc.


-- below some from there:
-- EXERCISE - A
-- (\xy.xy) 1 2 

-- (\x.\y.xy) 1 2       -- currying
-- (\y.1y) 2           [x:=1]
-- 1 2      [y := 2]


-- EXERCISE B

-- (/𝑥𝑦.𝑥𝑦)  (/z.a)   1
-- --        arg 1    arg 2

-- (/x./y.xy) (/z.a)  1     -- first we just curry

-- (/y.(/z.a) y)    1         [x := (/z.a)]
-- --** ***** --   arg 2 remains

-- ((/z.a) 1)                 [y := 1]

-- -- since there is no z we throw it away
-- a    -- left only with a







-- EXERCISE 1

-- (/𝑥𝑦.𝑥𝑥𝑦)(/𝑥1.𝑥1𝑦1)(/𝑥2.𝑥2𝑧)
-- (/x./y.xxy)


-- /x.(/y.xxy) (/𝑥1.𝑥1𝑦1)(/𝑥2.𝑥2𝑧)
-- [x := ((/𝑥1.𝑥1𝑦1))]
-- (/y.(/𝑥1.𝑥1𝑦1) (/𝑥1.𝑥1𝑦1) y) (/𝑥2.𝑥2𝑧)
-- [y := (/𝑥2.𝑥2𝑧)]
-- (/𝑥1.𝑥1𝑦1) (/𝑥1.𝑥1𝑦1) (/𝑥2.𝑥2𝑧)
-- [x1 := (/𝑥1.𝑥1𝑦1)]
-- ((/𝑥1.𝑥1𝑦1) y1) (/𝑥2.𝑥2𝑧)
-- [x1 := y1]
-- (y1 y1) (/x2.x2z)
-- [x2 := (/𝑥2.𝑥2𝑧)]
-- (((/𝑥2.𝑥2𝑧) z) y1) y1


-- repeat practice 
-- (/𝑥𝑦.𝑥𝑥𝑦)(/𝑥.𝑥𝑦)(/𝑥.𝑥𝑧)

-- (/𝑥𝑦.𝑥𝑥𝑦) (/𝑥1.𝑥1𝑦1) (/𝑥2.𝑥2𝑧)   -- going with brackets we have 2 arguments.   - Alpha equivalence used different variable names.
-- -------    arg 1 --    arg 2 

-- (/x/y.xxy) (/𝑥1.𝑥1𝑦1) (/𝑥2.𝑥2𝑧)   -- Not yet reducing but just currying explict

-- (/y.(/𝑥1.𝑥1𝑦1) (/𝑥1.𝑥1𝑦1) y)      (/𝑥2.𝑥2𝑧)               [x := (/𝑥1.𝑥1𝑦1)]   -- bindin x to the 1st arg
-- --** --------------------**  -only 1 arg (arg2 ) from before for y

-- ((/x1.x1y1) (/𝑥1.𝑥1𝑦1) (/𝑥2.𝑥2𝑧))        [y:= (/𝑥2.𝑥2𝑧)] -- bind y to argument (/𝑥2.𝑥2𝑧)
-- --*******--  arg 1       arg2

-- (/𝑥1.𝑥1𝑦1 y1)    (/𝑥2.𝑥2𝑧))        [x1 := (/𝑥1.𝑥1𝑦1)]    -- bind x1

-- (((/𝑥2.𝑥2𝑧) y1) y1)                  [x1 := (/𝑥2.𝑥2𝑧)]

-- (y1 z) y1                          [x2 := y1]        



-- EXAMPLE 2 
-- (/𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(/𝑚𝑛.𝑚)(/𝑝.𝑝)
-- (/x./y./z.xz(yz)) (/m./n.m) (/p.p)

-- [x:= (/m./n.m)]

-- (/y./z.(/m./n.m) z (yz))    (/p.p)

-- [y := (/p.p)]
-- /z.(/m./n.m)z ((/p.p)  z)

-- [m := z]
-- /z.(/n.z)



-- Ex 2 - redo to practice again - 

-- (/𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))   (/𝑚𝑛.𝑚)    (/𝑝.𝑝)
-- --              arg1        arg2 
-- (/x./y./z.𝑥𝑧(𝑦𝑧))    (/𝑚./𝑛.𝑚)    (/𝑝.𝑝)        -- No reduction yet Just currynig explicit

-- (/y./z.(/𝑚./𝑛.𝑚) z (yz))        ((/p.p))       -- Binding x to first argument (/𝑚./𝑛.𝑚)
-- --                              remaining arg
-- (/z.(/𝑚./𝑛.𝑚)    z    ((/p.p) z))          -- Bindng y to argument (/p.p)
-- -- now z does not having any to bind so we need to look internally 
-- internal1 withing z 
--     **-----**   arg1   ** arg2 **    
--  /z.(/n.z)     ((/p.p) z))        --  Binding m -- [m := z]
-- -- now we cannot reduce to n
-- /z.z  -- So will just reduce to z  -- the part (/n.z)     ((/p.p) z))  will reduce to z.




-- LESSONG learn - brackets are huge - need to bind from outermost .



-- Intermission: Equivalence exercises - 3 examples here.

-- 1.7 Evaluation is simplification






-- \xy.xxy

-- conFunc = \x y -> x x y 

-- (\xy.xxy) 1 2
-- (\x.\y.xxy) 1 2
-- (\y. 1 1 y) 2
-- 1 1 2

-- f = \x.\y.y   -- f x y = y

-- (\x.\y.y) 1 3

-- (\y.y) 3
-- 3

lambdaHaskx1 x = (\y z -> x z (y z))
-- lambdaHasky1 y = 
lambdaHask a b c = a c (b c)

b :: Int -> Int
b i = i + 1

a :: Int -> Int -> Int 
a n1 n2 = n1 + n2

c :: Int              
c = 10

-- *Main> lambdaHask a b c
-- 21


-- My own experimenting 
simpleLeftAssoc1 :: Int -> Int 
simpleLeftAssoc1 a  = a + 10        -- \a.a+10   of course in Lambda calculus i did not see direct values.


simpleLeftAssoc2 :: Int -> Int 
simpleLeftAssoc2 a  = a * 10


simpleLeftAssoc3 :: Float -> Float -> Float -> Float
simpleLeftAssoc3 a b c = a / b / c                    -- -- Left Associative by default



simpleLeftAssoc4 :: Float -> Float -> Float -> Float
simpleLeftAssoc4 a b c = a / ( b / c ) 

-- simpleLeftAssoc5 :: Float -> Float -> Float -> Float
-- simpleLeftAssoc5 a b c = let d = a / b
--                          if d > c 
--                          then d
--                          else c  


-- 1000 10 5 = 1000/ 10 = 100 / 5 = 20.    -- Left Associative correct answer - simpleLeftAssoc3 by default
-- 1000 ((10 5 )) = 1000 2 = 500.     -- if you want right association we need brackets. have to go with simpleLeftAssoc4


-- elem = simpleLeftAssoc1 simpleLeftAssoc2 50   -- gives a diff error 

elem1 = simpleLeftAssoc1 (simpleLeftAssoc2 50)

main :: IO ()
main = do
    putStrLn (" left assoc total elem1 = " ++ show(elem1))
    -- putStrLn (" left assoc total elem = " ++ show(elem))
    -- putStrLn (" left assoc total elem1 = " ++ show(simpleLeftAssoc1 simpleLeftAssoc2 50)) This gives a correct error 


-- Error for -- putStrLn (" left assoc total elem1 = " ++ show(simpleLeftAssoc1 simpleLeftAssoc2 50)) This gives a correct error

-- purple-1-Lambda.hs:19:52: error:
--     • Couldn't match expected type ‘t0 -> a0’ with actual type ‘Int’
--     • The function ‘simpleLeftAssoc1’ is applied to two arguments,
--       but its type ‘Int -> Int’ has only one
--       In the first argument of ‘show’, namely
--         ‘(simpleLeftAssoc1 simpleLeftAssoc2 50)’
--       In the second argument of ‘(++)’, namely
--         ‘show (simpleLeftAssoc1 simpleLeftAssoc2 50)’
--    |
-- 19 |     putStrLn (" left assoc total elem1 = " ++ show(simpleLeftAssoc1 simpleLeftAssoc2 50))
--    |                                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- purple-1-Lambda.hs:19:69: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘Int -> Int’
--     • Probable cause: ‘simpleLeftAssoc2’ is applied to too few arguments
--       In the first argument of ‘simpleLeftAssoc1’, namely
--         ‘simpleLeftAssoc2’
--       In the first argument of ‘show’, namely
--         ‘(simpleLeftAssoc1 simpleLeftAssoc2 50)’
--       In the second argument of ‘(++)’, namely
--         ‘show (simpleLeftAssoc1 simpleLeftAssoc2 50)’
--    |
-- 19 |     putStrLn (" left assoc total elem1 = " ++ show(simpleLeftAssoc1 simpleLeftAssoc2 50))