BIN_DIR=../bin

all: prob1 server prob1sm clearskies

server:
	ghc --make -o $(BIN_DIR)/server Server.hs -main-is Server -O
simul:
	ghc --make -o $(BIN_DIR)/simul Simulation.hs -main-is Simulation -O3 -XBangPatterns

prob1:
	ghc --make -o $(BIN_DIR)/prob1 Prob1.hs -main-is Prob1 -O

prob1sm:
	ghc --make -o $(BIN_DIR)/prob1sm Prob1SM.hs -main-is Prob1SM -O

visualizer:
	ghc --make -o $(BIN_DIR)/visualizer Visualizer.hs -main-is Visualizer -O
clearskies:
	ghc --make -o $(BIN_DIR)/clearskies ClearSkies.hs -main-is ClearSkies -O
main:
	ghc --make -o $(BIN_DIR)/main Main.hs -main-is Main -O
search:
	ghc --make -o $(BIN_DIR)/search Search.hs -O3 -prof -auto-all -caf-all
clean:
	rm *.hi *.o


.PHONY: all clean
