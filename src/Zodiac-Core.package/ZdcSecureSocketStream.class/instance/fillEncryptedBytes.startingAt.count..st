private in
fillEncryptedBytes: bytes startingAt: offset count: count
	"Read encrypted bytes from the network. Do not wait."
	
	^ super fillBytes: bytes startingAt: offset count: count