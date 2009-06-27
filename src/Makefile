BIN_DIR=../bin

all: prob1 server prob1

server:
	ghc --make -o $(BIN_DIR)/server Server.hs -main-is Server -O

prob1:
	ghc --make -o $(BIN_DIR)/prob1 Prob1.hs -main-is Prob1

main:
	ghc --make -o $(BIN_DIR)/main Main.hs -main-is Main

clean:
	rm *.hi *.o


.PHONY: all clean
