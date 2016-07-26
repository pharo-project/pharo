primitives-accessing
replaceFrom: start "<Integer>" to: stop "<Integer>" with: replacement "<Alien | indexableByteSubclass>" startingAt: repStart "<Integer>" "^<self>"
	<primitive: 'primAlienReplace' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed