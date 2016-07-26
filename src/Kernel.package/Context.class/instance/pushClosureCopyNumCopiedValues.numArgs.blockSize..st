instruction decoding
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Simulate the action of a 'closure copy' bytecode whose result is the
	 new BlockClosure for the following code"
	| copiedValues |
	numCopied > 0
		ifTrue: [
			copiedValues := Array new: numCopied.
			numCopied to: 1 by: -1 do: [ :i |
				copiedValues at: i put: self pop ]]
		ifFalse: [ copiedValues := nil ].
	self push: (BlockClosure
				outerContext: self
				startpc: pc
				numArgs: numArgs
				copiedValues: copiedValues).
	self jump: blockSize