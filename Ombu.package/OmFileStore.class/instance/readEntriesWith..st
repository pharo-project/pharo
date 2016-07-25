enumerating
readEntriesWith: aBlockClosure

	| readStream |
	readStream := self fileReference readStream ascii.

	[ ^ aBlockClosure 
			value: self newEntryReader 
			value: readStream ] 
			ensure: [ readStream close ]