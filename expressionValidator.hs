expression :: String -> Bool
expression "" = True
expression string = validateExpression string [] ")}]"



validateExpression :: String -> String -> String -> Bool
validateExpression [] [] _ = True
validateExpression [] (x:_) _ = False
validateExpression (first:[]) [] _ = False
validateExpression (first:rest) stack closingExpressions 
                | elem first closingExpressions = if isMatching first stack
                                                    then validateExpression rest popStack closingExpressions
                                                    else False
                                                                            
                | otherwise = validateExpression rest pushStack closingExpressions
                where popStack = pop stack
                      pushStack = push stack first



isMatching :: Char -> String -> Bool
isMatching current [] = False
isMatching current (first:rest)
        | current == ')' = first == '('
        | current == '}' = first == '{'
        | current == ']' = first == '['
        | otherwise = False




push :: [a] -> a -> [a]
push elements newElement = (:) newElement elements

pop :: [a] -> [a]
pop [] = error "Not valid for empty string"
pop (first:rest) = rest

