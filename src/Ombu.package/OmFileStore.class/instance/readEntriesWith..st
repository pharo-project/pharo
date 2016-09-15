enumerating
readEntriesWith: aBlockClosure

	| readStream |
	readStream := self fileReference readStream ascii.

	[ ^ aBlockClosure 
			value: readStream ] 
			ensure: [ readStream close ]