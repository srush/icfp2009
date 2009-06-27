module Tests where 
import Instructions
import OpParser
import Interpreter
import Test.HUnit
import Satellite

testReal = TestCase $ do
              (ops, mem) <- readBin file
              initState <- setup ops mem 
              ostate <- runRound ops [] initState
              return () 


testOutput = TestCase $ do
              (ops, mem) <- readBin file
              initState <- setup ops mem 
              mem <- memToList initState
              --mapM_ (putStrLn . show) $ zip mem [0..] 
              --mapM_ (putStrLn . show) $ zip ops [0..]
 
              ostates <-  runRounds ops ([(16000, 1001)] : (repeat [(2, 1.000),(3,1.000)])) initState 100

              --mem2 <- memToList ostate
              --print $ show mem2               
              --mapM_ showPorts ostates
              -- mapM_ showPorts ostates
              ports <- mapM getPorts ostates
              print $ show $ map portsToPos ports  
              return () 
    where showPorts st = do
                          ls <- portToList st
                          putStrLn $ show ls
          getPorts st = do 
            portToMap st 

runReal = TestList $ [testOutput]