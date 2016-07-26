primitives-accessing
copyInto: aByteObject "<Alien | indexableByteSubclass>" from: start "<Integer>" to: stop "<Integer>" in: replacement "<Alien | indexableByteSubclass>" startingAt: repStart "<Integer>" "^<self>"
	<primitive: 'primAlienReplace' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed