-- represents a single key-value pair in a JSON object
-- ex:          "states" : [1,2,3]
type JsonPair = (String, JsonData)

-- type constructor for JSON data types, including wrapped ints and strings
-- note, this is a simplified JSON format since some datatypes are unneeded
data JsonData = JsonObject [JsonPair] | JsonArray [JsonData]
                  | JsonInt Integer | JsonString String
       deriving Show

-- tokens for use by the JSON lexer
data Token = Comma | Quote | LBrack | RBrack | Colon
                  | LBrace | RBrace | CSym Char | Par JsonData
       deriving Show

-- the JSON lexer
lexer :: String -> [Token]
-- empty string
lexer [] = []
-- whitespace
lexer (' ':xs)  = lexer xs
lexer ('\n':xs) = lexer xs
lexer ('\r':xs) = lexer xs
-- symbols
lexer (':':xs) = Colon  : lexer xs
lexer (',':xs) = Comma  : lexer xs
lexer ('[':xs) = LBrack : lexer xs
lexer (']':xs) = RBrack : lexer xs
lexer ('{':xs) = LBrace : lexer xs
lexer ('}':xs) = RBrace : lexer xs
lexer ('"':xs) = Quote  : lexer xs
-- otherwise we push this as a generic char
lexer (c:xs) = CSym c : lexer xs

-- parser (starts empty shift reducer)
parse :: [Token] -> JsonData
parse x = let reduced = sr [] x
              in case reduced !! 0 of
                   Par (job@(JsonObject s)) -> job
                   otherwise                -> jsonError $ "parse error on JSON input"

-- the shift reducer
sr :: [Token] -> [Token] -> [Token]
-- reduce
-- json integer (NOT preceded by a quote & start with numeric):
sr (CSym n:qs) p | noQuotes qs && isNumeric n =
            sr (Par (JsonInt (read (n : takeNumericCSyms p))):qs) (dropNumericCSyms p)
-- empty json string:
sr (Quote:Quote:qs) p = sr (Par (JsonString []):qs) p
-- start of a json string:
sr (CSym a:Quote:qs) p = sr (Par (JsonString [a]):Quote:qs) p
-- continue an unclosed json string:
sr (CSym a:Par (JsonString s):Quote:qs) p = sr (Par (JsonString (s ++ [a])):Quote:qs) p
-- end of a json string (drop quotes):
sr (Quote:Par (JsonString s):Quote:qs) p = sr (Par (JsonString s):qs) p
-- add key-value pair to object on stack
sr (Par (JsonObject d):Comma:Par (JsonObject e):LBrace:qs) p = sr (Par (JsonObject (e ++ d)):LBrace:qs) p
-- json key-value pair (a key followed by some data object)
sr (Par j:Colon:Par (JsonString s):qs) p = sr (Par (JsonObject [(s, j)]):qs) p
-- finish object (drop braces):
sr (RBrace:Par job@(JsonObject kv):LBrace:qs) p = sr (Par job:qs) p
-- start json array:
sr (Par j:LBrack:qs) (p:ps) = case j of
                jar@(JsonArray a) -> sr (p:Par jar:LBrack:qs) ps -- skip if already an array
                otherwise         -> sr (Par (JsonArray [j]):LBrack:qs) (p:ps) -- start array with 1 elem
-- add item to json array:
sr (Par j:Comma:Par (JsonArray ja):LBrack:qs) p = sr (Par (JsonArray (ja ++ [j])):LBrack:qs) p
-- finish json array:
sr (RBrack:Par jar@(JsonArray ja):LBrack:qs) p = sr (Par jar:qs) p
-- shift
sr q (p:ps) = sr (p:q) ps
-- finish
sr all [] = all

-- helper for finding numbers
isNumeric :: Char -> Bool
isNumeric c = c `elem` ['0'..'9']

-- helper to check for Quotes on the stack
noQuotes :: [Token] -> Bool
noQuotes [] = True
noQuotes (Quote:xs) = False
noQuotes (_:xs) = noQuotes xs

-- helper to take CSym tokens while the wrapped char is numeric
takeNumericCSyms :: [Token] -> String
takeNumericCSyms (CSym n:xs) = if isNumeric n then n : takeNumericCSyms xs else []
takeNumericCSyms _             = []

-- helper to drop numeric CSym tokens
dropNumericCSyms :: [Token] -> [Token]
dropNumericCSyms xs = drop (length $ takeNumericCSyms xs) xs

-- lookup function to find a key-value pair in a JSON object
lookupKey :: String -> JsonData -> Maybe JsonData
lookupKey s (JsonObject job) =
         case lookup s job of
             Just jda -> Just jda
             Nothing  -> let fil = filter (\x -> case x of
                                       Just j    -> True
                                       otherwise -> False)
                                   (map (\y -> lookupKey s (snd y)) job)
                           in if length fil > 0 then fil !! 0 else Nothing
lookupKey s (JsonArray jar) =
         let fil = filter (\x -> case x of
                        Just j    -> True
                        otherwise -> False)
                   (map (\y -> lookupKey s y) jar)
           in if length fil > 0 then fil !! 0 else Nothing
lookupKey _ _ = Nothing

-- turing machine types
type State = Integer
type Symbol = Char

type States = [State]
type Gamma = [Symbol]
type Blank = Symbol
type Input = Gamma

data HeadMove = L | R
    deriving Show
type Transition = ((State, Symbol), (State, Symbol, HeadMove))
type Delta = [Transition]

type StartState = State
type Acceptors = States

-- constructor for machine 7-tuple
data TuringMachine = TuringMachine {
    getStates :: States,
    getGamma :: Gamma,
    getBlank :: Blank,
    getInput :: Input,
    getDelta :: Delta,
    getStart :: StartState,
    getFinals :: Acceptors
}

instance Show TuringMachine where
    show = showMachine
    
-- helper method to display machine as string
showMachine :: TuringMachine -> String
showMachine (TuringMachine q g b i d s f) =
      "States: " ++ show q ++ "\n" ++
      "Tape alphabet: " ++ show g ++ "\n" ++
      "Blank symbol: " ++ [b] ++ "\n" ++
      "Input alphabet: " ++ show i ++ "\n" ++
      "Transition function:\n" ++ 
          concat (map showDelta d) ++
      "Start state: " ++ show s ++ "\n" ++
      "Final states: " ++ show f

-- helper method to display transitions as string
showDelta :: Transition -> String
showDelta ((f, r), (t, w, m)) =
      "    (" ++ show f ++ ", " ++ [r] ++ ") -> ("
              ++ show t ++ ", " ++ [w] ++ ", " ++ show m ++ ")\n"

-- tape, represented as a list of symbols
type Tape = [Symbol]
-- machine state, which includes the tape, state, and head position
type MachineState = (Tape, State, Int)

-- helper method to display the machine state as string
showMachineState :: MachineState -> String
showMachineState (t, s, h) = "    " ++ (concat $ take h (cycle ["     "]))
                     ++ "  v   (state = " ++ (show s) ++ ")\n" ++ (showTape t)

-- helper method to display the tape as string
showTape :: Tape -> String
showTape tape = ".. ]" ++ concat (map (\x -> "[ " ++ [x] ++ " ]") tape) ++ "[ .."

-- helper method to trim the tape (clear leading/trailing blanks)
trimTape :: Tape -> Symbol -> Tape
trimTape tape b =
    let tail = dropWhile (\x -> x == b) tape
        endLen = length (takeWhile (\x -> x == b) (reverse tape))
     in take (length tail - endLen) tail
     
-- creates machine from JSON input
createTuring :: JsonData -> TuringMachine
createTuring job@(JsonObject prs) =
       -- find the states key
   let q = case lookupKey "states" job of
              Just (JsonArray j) -> j
              Nothing            -> jsonError "\"states\" is not defined"
       -- find the gamma key
       g = case lookupKey "gamma" job of
              Just (JsonArray j) -> j
              Nothing            -> jsonError "\"gamma\" is not defined"
       -- find the blank key
       b = case lookupKey "blank" job of
              Just (JsonString j) -> j
              Nothing             -> jsonError "\"blank\" is not defined"
       -- find the input key
       i = case lookupKey "input" job of
              Just (JsonArray j) -> j
              Nothing            -> jsonError "\"input\" is not defined"
       -- find the transitions key
       d = case lookupKey "transitions" job of
              Just (JsonArray j) -> j
              Nothing            -> jsonError "\"transitions\" is not defined"
       -- find the start key
       s = case lookupKey "start" job of
              Just (JsonInt j) -> j
              Nothing          -> jsonError "\"start\" is not defined"
       -- find the finals key
       f = case lookupKey "finals" job of
              Just (JsonArray j) -> j
              Nothing            -> jsonError "\"finals\" is not defined"
       -- fold up the q array into a list of ONLY integers
       states = foldr (\x acc -> case x of {
                        JsonInt n -> n : acc;
                        otherwise -> jsonError "states must be integers"} ) [] q
       -- fold up the g array into a list of ONLY strings
       gamma  = foldr (\x acc-> case x of {
                        JsonString s -> s : acc;
                        otherwise      -> jsonError "tape symbols must be strings"} ) [] g
       -- fold up the i array into a list of ONLY strings
       input  = foldr (\x acc -> case x of {
                        JsonString s -> s : acc;
                        otherwise      -> jsonError "input symbols must be strings"} ) [] i
       -- fold up the f array into a list of ONLY integers
       finals = foldr (\x acc -> case x of {
                        JsonInt n -> n : acc;
                        otherwise -> jsonError "states must be integers"}) [] f
       -- use helper function to fold up the transitions
       trans  = map toDeltaFunc d
   -- construct (incomplete) machine and sanitize it to return a TM
   in sanitize (states, gamma, b, input, trans, s, finals)
-- error if the input JSON data is not an object
createTuring _ = jsonError "top-level data should be a JSON object"

{- helper function to check/sanitize json input - converts strings to chars,
   checks that input alphabet is a subset of gamma, F is a subset of Q, etc. -}
sanitize :: (States, [String], String, [String], Delta, State, States) -> TuringMachine
sanitize im@(q, g, b, i, d, s, f) =
   -- check that all final states occur in Q
   let qfcheck = False `elem` (map (\x -> x `elem` q) f) || (not $ s `elem` q)
   -- check that all input symbols occur in gamma
       gicheck = False `elem` (map (\x -> x `elem` g) i)
   -- "chop" all symbols to convert to char (error if len > 1)
       glchop = map (\x -> if length x == 1 then x !! 0 else jsonError "tape symbols must be one character") g
       ilchop = map (\x -> if length x == 1 then x !! 0 else jsonError "input symbols must be one character") i
       bchop = if length b == 1 then b !! 0 else jsonError "blank symbol must be one character"
   -- now the data matches the TM constructor, return finished TM
   in TuringMachine q glchop bchop ilchop d s f

-- helper to parse transition function JSON objects
toDeltaFunc :: JsonData -> Transition
toDeltaFunc job@(JsonObject prs) =
       -- find the from key
   let f = case lookupKey "from" job of
              Just (JsonInt j) -> j -- from should be an int (state)
              otherwise        -> jsonError "in transition, \"from\" must be an integer"
       -- find the read key
       r = case lookupKey "read" job of -- read should be a 1-char string (symbol)
              Just (JsonString j) -> if length j == 1 then j !! 0 else jsonError "bad symbol in transition function"
              otherwise           -> jsonError "in transition, \"read\" must be a string"
       -- find the to key
       t = case lookupKey "to" job of
              Just (JsonInt j) -> j -- to should be an int (state)
              otherwise        -> jsonError "in transition, \"to\" must be an integer"
       -- find the write key
       w = case lookupKey "write" job of -- write should be a 1-char string (symbol)
              Just (JsonString j) -> if length j == 1 then j !! 0 else jsonError "bad symbol in transition function"
              otherwise           -> jsonError "in transition, \"write\" must be a string"
       -- find the move key
       m = case lookupKey "move" job of -- should be an "L" or "R"
              Just (JsonString m) | m == "L" -> L
                                  | m == "R" -> R
              otherwise           -> jsonError "in transition, \"move\" must be \"L\" or \"R\""
    -- the final transition construction
    in ((f, r), (t, w, m))
-- error if the transition JSON data is not an object 
toDeltaFunc _ = jsonError "transitions must be encoded as JSON objects"

-- helper to throw JSON input error with message
jsonError message = error $ "Malformed JSON input; " ++ message

{-
    THE TURING MACHINE EVALUATION
-}

-- create the initial machine state
-- tape = input, state = start, head = 0
initialState :: TuringMachine -> [Symbol] -> MachineState
initialState tm input = (input, getStart tm, 0)

-- run machine by recursively performing steps, until termination
runTuring :: TuringMachine -> MachineState -> MachineState
runTuring tm ms =
    -- take one execution step from the current state
    let (newState, term) = stepTuring tm ms
    -- if the step did not result in termination, recurse
    in if term then ms else runTuring tm newState

{- run a step of the machine from the given machine state
   returns the new machine state after the step, plus a
   boolean, which is true if the machine has terminated -}
stepTuring :: TuringMachine -> MachineState -> (MachineState, Bool)
stepTuring tm ms@(tape, state, head) = -- first find the correct transition
   -- read the tape symbol at the head index
   let read = tape !! head
       -- use a helper to search for a transition from this state
       find = findTransition state read (getDelta tm)
   in case find of    -- see if a transition was found
       Nothing               -> ((tape, state, head), True) -- if no transition, terminate here
       Just (p1, (to, w, m)) ->                             -- else, do transition and update state
                -- adjust head left or right
                let newHead = case m of {
                           (L) -> head - 1;
                           (R) -> head + 1}
                    -- use helper to write to the tape
                    writTape = replace tape head w
                    -- extend tape at front or end to simulate infinite tape
                    adjTape = shift writTape (getBlank tm) newHead
                    -- if head moved out of bounds, move it back in
                    adjHead = if newHead < 0 then 0 else newHead
                -- return the new machine state (and no termination)
                 in ((adjTape, to, adjHead), False)

-- check if machine is in a accepting state
accepted :: TuringMachine -> MachineState -> Bool
accepted tm ms@(tape, state, head) = state `elem` (getFinals tm)

-- helper to shift tape (add blanks at front/end) to simulate infinite tape
shift :: Tape -> Symbol -> Int -> Tape
-- add blanks at the front if needed
shift tape b n | n < 0 = take (0 - n) (cycle [b]) ++ tape
-- add blanks at the ened if needed
shift tape b n | n >= length tape = tape ++ (take (length tape - n + 1) (cycle [b]))
shift tape _ _ = tape

-- helper to "write" to a slot in the tape
replace :: Tape -> Int -> Symbol -> Tape
replace tape h s = take h tape ++ (s : (drop (h + 1) tape))

-- helper to find the relevant transition from delta function
findTransition :: State -> Symbol -> Delta -> Maybe Transition
findTransition _ _ [] = Nothing
findTransition from read (d@((f, r), p2):ds) =
        if from == f && read == r then Just d else findTransition from read ds

-- user IO
main :: IO ()
-- launch repl with nothing loaded
main = repl Nothing Nothing

-- maintains current loaded machine (if loaded) and its state (if running)
repl :: Maybe TuringMachine -> Maybe MachineState -> IO ()
repl maybeMachine maybeMachState = do
  case maybeMachine of
    -- if no machine is loaded...
    Nothing -> do
      putStrLn $ barThing ++ "No machine is loaded."
      putStrLn "Select an option:\n1) Help\n2) Load machine\nQ) Quit"
      req <- getLine
      -- user decision
      case req of
        ('1':xs)  -> putStrLn (barThing ++ helpString) >> repl Nothing Nothing -- restart
        ('2':xs)  -> do
        -- load JSON file, attempt to parse and create machine
          putStrLn "Enter filename:"
          fname <- getLine
          filec <- readFile fname
          let m = createTuring $ parse $ lexer filec
          -- launch repl with machine loaded but unstarted
          repl (Just m) Nothing
        ('Q':xs)  -> return ()
        otherwise -> putStrLn "Enter a valid option." >> repl Nothing Nothing
    -- a machine is loaded!
    Just tm -> case maybeMachState of
      -- machine has not been started yet
      Nothing -> do
        putStrLn $ barThing ++ "A machine is loaded."
        putStrLn "Select an option:\n1) View machine\n2) Provide input\nQ) Quit"
        req <- getLine
        case req of
          ('1':xs)  -> putStrLn (barThing ++ (show tm)) >> repl (Just tm) Nothing
          ('2':xs)  -> do
          -- get and set input for the machine
            putStrLn "Enter input string:"
            input <- getLine
            -- make sure input string is valid
            let unsan = False `elem` (map (\x -> x `elem` (getInput tm)) input)
            if unsan then -- bad input, retry
               putStrLn (barThing ++ "Invalid input; symbols must be in the machine's input alphabet.") >> repl (Just tm) Nothing
               -- good input; launch repl with machine loaded and tape set up
               else repl (Just tm) (Just (initialState tm input))
          ('Q':xs)  -> return ()
          otherwise -> putStrLn "Enter a valid option." >> repl (Just tm) Nothing
      -- machine is currently running
      Just ms@(t, s, h) -> do
        -- lookahead one step to see if TM has terminated
        let (stepState, term) = stepTuring tm ms
        case term of
          -- not terminated, ask what to do next
          False -> do
            putStrLn $ barThing ++ "The machine is running, with this tape:"
            putStrLn $ showMachineState ms
            putStrLn "Select an option:\n1) Run all\n2) Step once\nQ) Quit"
            req <- getLine
            -- do run or step, launch repl with the resulting state
            case req of
              ('1':xs)  -> repl (Just tm) (Just (runTuring tm ms))
              ('2':xs)  -> repl (Just tm) (Just stepState)
              ('Q':xs)  -> return ()
              otherwise -> putStrLn "Enter a valid option." >> repl (Just tm) (Just ms)
          -- terminated, ask to reload or restart
          True -> do
            putStrLn $ barThing ++ "The machine has terminated, with this tape:"
            putStrLn $ showTape (trimTape t (getBlank tm))
            putStrLn $ "* The input was " ++ if accepted tm ms then "accepted!" else "not accepted."
            putStrLn "Select an option:\n1) Reload\n2) Restart\nQ) Quit"
            req <- getLine
            case req of
              ('1':xs)  -> repl (Nothing) (Nothing) -- unload machine
              ('2':xs)  -> repl (Just tm) (Nothing) -- restart at initial state
              otherwise -> putStrLn "Bye!" >> return ()

-- the help message
helpString :: String
helpString = "To load a Turing machine, you need a JSON file with the machine encoded in this format:\n"
  ++ "\t{\"states\":[<int>,...],\"gamma\":[<char>,...],\"blank\":<char>,\"input\":[<char>,...],\n"
  ++ "\t\"transitions\":[{\"from\":<int>,\"read\":<char>,\n\t\t\"to\":<int>,\"write\":<char>,\"move\":<\"L\" or \"R\">},...],\n"
  ++ "\t\"start\":<int>,\"finals\":[<int>,...]}\n"
  ++ "...where each state is an integer and each symbol is a one-character string.\n"
  ++ "After loading, you can provide an input string and simulate the machine step-by-step.\n"
  ++ "At any point, you can view the contents of the machine's tape.\n"
  ++ "If the machine terminates, it will display whether the input was accepted."

-- the bar thing
barThing :: String
barThing = "~~~~~~~~~~~~~~~\n"