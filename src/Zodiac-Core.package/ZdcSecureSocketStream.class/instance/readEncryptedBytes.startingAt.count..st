private in
readEncryptedBytes: bytes startingAt: offset count: count
	"Read encrypted bytes from the network. Wait if necessary."
	
	| result |
	result := super fillBytes: bytes startingAt: offset count: count.
	^ result = 0
		ifTrue: [
			self socketWaitForData. "when successful, recurse, else signal exception"
			self readEncryptedBytes: bytes startingAt: offset count: count ]
		ifFalse: [
			result ]