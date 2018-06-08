all:
	g++ -g main.cpp decode.cpp thumbsim_driver.cpp parse.cpp execute.cpp -o thumbsim

run:
	./thumbsim -i -d -s -c 256 -f inputs/bytetest.sim

clean:
	rm -rf ./*.o ./thumbsim
