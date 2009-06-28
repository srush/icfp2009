BIN_DIR=../bin

all: prob1 server main

server:
	ghc --make -o $(BIN_DIR)/server Server.hs -main-is Server -O

prob1:
	ghc --make -o $(BIN_DIR)/prob1 Prob1.hs -main-is Prob1 -O

main:
	ghc --make -o $(BIN_DIR)/main Main.hs -main-is Main -O

visualizer:
	ghc --make -o $(BIN_DIR)/visualizer Visualizer.hs -main-is Visualizer -O

clean:
	rm *.hi *.o


.PHONY: all clean
