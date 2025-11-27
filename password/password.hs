module Password where

-- PwdOP takes a string and would return some value and new password (string)
newtype PwdOp a = PwdOp(String -> (a, String))

instance Functor PwdOp where
    fmap f (PwdOp op) =
        PwdOp (\pwd ->
            let (a, pwd1) = op pwd
            in (f a, pwd1))

instance Applicative PwdOp where
    pure x = PwdOp (\pwd -> (x, pwd))
    (PwdOp pf) <*> (PwdOp pa) =
        PwdOp (\pwd ->
            let (f, pwd1) = pf pwd
                (a, pwd2) = pa pwd1
            in (f a, pwd2))
            
instance Monad PwdOp where
    return x = PwdOp (\pwd -> (x, pwd))

    (PwdOp op) >>= f = PwdOp (\pwd -> 
        let (a, pwd1) = op pwd 
            PwdOp op2 = f a 
        in op2 pwd1)

setPassword :: String -> PwdOp ()
-- Your code here
setPassword newpwd = PwdOp(\_ -> ((), newpwd))

checkPassword :: String -> PwdOp Bool
-- Your code here
checkPassword ip = PwdOp(\pwd -> (ip == pwd, pwd))

runPwdOp :: PwdOp a -> a
-- Your code here
runPwdOp (PwdOp op) = let (result, pwd) = op "" in result





