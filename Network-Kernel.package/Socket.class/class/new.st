instance creation
new
	"Return a new, unconnected Socket. Note that since socket creation may fail, it is safer to use the method createIfFail: to handle such failures gracefully; this method is primarily for backward compatibility and may be disallowed in a future release."
	"Note: The default creates a TCP socket - this is also backward compatibility."
	^self newTCP