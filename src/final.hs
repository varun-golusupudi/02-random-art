-- | 'assoc' searches for a key in a list of key-value pairs.
-- If the key is found, it returns the associated value.
-- If the key is not found, it returns the default value 'def'.

assoc :: Int -> String -> [(String, Int)] -> Int
assoc def key [] = def  -- Base case: if the list is empty, return the default value.
assoc def key ((k, v):xs)  -- Recursive case: check the first pair (k, v) in the list.
  | key == k  = v       -- If 'key' matches 'k', return the associated value 'v'.
  | otherwise = assoc def key xs  -- Otherwise, recursively check the rest of the list.


-- | 'listReverseTR' reverses a list using tail recursion.
-- It uses an accumulator to efficiently build the reversed list.

listReverseTR :: [a] -> [a]
listReverseTR xs = helper xs []  -- Calls helper function with an empty accumulator.
  where
    helper [] acc = acc  -- Base case: return the accumulator when the list is empty.
    helper (y:ys) acc = helper ys (y:acc)  -- Recursive case: prepend to accumulator and recurse.


-- | 'doubleEveryOtherTR' doubles every second element in the list.
-- It keeps track of whether to double using a boolean flag.

doubleEveryOtherTR :: [Integer] -> [Integer]
doubleEveryOtherTR xs = helper xs False []  -- Calls helper function with initial flag as False.
  where
    helper [] _ acc = reverse acc  -- Base case: reverse the accumulator to restore order.
    helper (y:ys) toggle acc =
      helper ys (not toggle) ((if toggle then 2*y else y) : acc)  -- If toggle is True, double the element.


-- | 'sumListTR' computes the sum of a list using tail recursion.
-- It accumulates the sum instead of using implicit recursion.

sumListTR :: [Integer] -> Integer
sumListTR xs = helper xs 0  -- Calls helper function with initial sum as 0.
  where
    helper [] acc = acc  -- Base case: return the accumulated sum.
    helper (y:ys) acc = helper ys (acc + y)  -- Recursive case: add element to accumulator and recurse.


--------------------------------------------------------------------------------
-- | Data Type for Random Art Expressions --------------------------------------
--------------------------------------------------------------------------------

-- | 'Expr' represents mathematical expressions used to generate images.
-- It includes variables, trigonometric functions, arithmetic operations,
-- and threshold-based conditional logic.

data Expr
  = VarX  -- Represents variable 'x'.
  | VarY  -- Represents variable 'y'.
  | Sine    Expr  -- Represents 'sin(pi * expr)'.
  | Cosine  Expr  -- Represents 'cos(pi * expr)'.
  | Average Expr Expr  -- Represents '((expr1 + expr2) / 2)'.
  | Times   Expr Expr  -- Represents 'expr1 * expr2'.
  | Thresh  Expr Expr Expr Expr  -- Represents '(expr1 < expr2 ? expr3 : expr4)'.
  | Average3 Expr Expr Expr  -- New operator: computes the average of three expressions.
  | Average4 Expr Expr Expr Expr  -- New operator: computes the average of four expressions.
  deriving (Show)


-- | 'exprToString' converts an expression into a human-readable string.
-- It follows the syntax rules of mathematical expressions.

exprToString :: Expr -> String
exprToString VarX = "x"  -- Variable 'x'.
exprToString VarY = "y"  -- Variable 'y'.
exprToString (Sine e) = "sin(pi*" ++ exprToString e ++ ")"  -- Convert sine expression.
exprToString (Cosine e) = "cos(pi*" ++ exprToString e ++ ")"  -- Convert cosine expression.
exprToString (Average e1 e2) = "((" ++ exprToString e1 ++ "+" ++ exprToString e2 ++ ")/2)"  -- Convert average.
exprToString (Times e1 e2) = exprToString e1 ++ "*" ++ exprToString e2  -- Convert multiplication.
exprToString (Thresh e1 e2 e3 e4) =
  "(" ++ exprToString e1 ++ "<" ++ exprToString e2 ++ "?" ++ exprToString e3 ++ ":" ++ exprToString e4 ++ ")"  -- Convert conditional.
exprToString (Average3 e1 e2 e3) = "((" ++ exprToString e1 ++ "+" ++ exprToString e2 ++ "+" ++ exprToString e3 ++ ")/3)"  -- Convert three-way average.
exprToString (Average4 e1 e2 e3 e4) = "((" ++ exprToString e1 ++ "+" ++ exprToString e2 ++ "+" ++ exprToString e3 ++ "+" ++ exprToString e4 ++ ")/4)"  -- Convert four-way average.


-- | 'eval' evaluates an expression at a given (x, y) coordinate.
-- It ensures that all values stay within the range [-1.0, 1.0].

eval :: Double -> Double -> Expr -> Double
eval x _ VarX = x  -- If the expression is VarX, return x.
eval _ y VarY = y  -- If the expression is VarY, return y.
eval x y (Sine e) = sin (pi * eval x y e)  -- Evaluate sine(pi * expr).
eval x y (Cosine e) = cos (pi * eval x y e)  -- Evaluate cosine(pi * expr).
eval x y (Average e1 e2) = (eval x y e1 + eval x y e2) / 2  -- Evaluate average.
eval x y (Times e1 e2) = eval x y e1 * eval x y e2  -- Evaluate multiplication.
eval x y (Thresh e1 e2 e3 e4) = if eval x y e1 < eval x y e2 then eval x y e3 else eval x y e4  -- Evaluate conditional.
eval x y (Average3 e1 e2 e3) = (eval x y e1 + eval x y e2 + eval x y e3) / 3  -- Evaluate three-way average.
eval x y (Average4 e1 e2 e3 e4) = (eval x y e1 + eval x y e2 + eval x y e3 + eval x y e4) / 4  -- Evaluate four-way average.


-- | 'build' generates a random expression with a given depth.
-- It randomly chooses mathematical operations to create complex patterns.

build :: Int -> Expr
build 0
  | r < 5 = VarX  -- If random value is below 5, return VarX.
  | otherwise = VarY  -- Otherwise, return VarY.
  where
    r = rand 10  -- Generate a random number between 0 and 9.

build d = case r of
  0 -> Sine (build (d - 1))  -- Generate a sine expression.
  1 -> Cosine (build (d - 1))  -- Generate a cosine expression.
  2 -> Average (build (d - 1)) (build (d - 1))  -- Generate an average expression.
  3 -> Times (build (d - 1)) (build (d - 1))  -- Generate a multiplication expression.
  4 -> Average3 (build (d - 1)) (build (d - 1)) (build (d - 1))  -- Generate three-way average.
  5 -> Average4 (build (d - 1)) (build (d - 1)) (build (d - 1)) (build (d - 1))  -- Generate four-way average.
  _ -> Thresh (build (d - 1)) (build (d - 1)) (build (d - 1)) (build (d - 1))  -- Generate a conditional.
  where
    r = rand 6  -- Generate a random number between 0 and 5.
