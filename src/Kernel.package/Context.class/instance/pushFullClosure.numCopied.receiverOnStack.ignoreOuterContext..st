instruction decoding
pushFullClosure: compiledBlock numCopied: numCopied receiverOnStack: onStack ignoreOuterContext: ignore
	| copiedValues cls |
	copiedValues := (1 to: numCopied) collect: [ :i | self pop ].
	self push: (cls := (FullBlockClosure new: numCopied) 
		outerContext: (ignore ifFalse: [self]);
		receiver: (onStack ifTrue: [ self pop ] ifFalse: [ receiver ]);
		numArgs: compiledBlock numArgs;
		compiledBlock: compiledBlock;
		yourself).
	copiedValues size to: 1 by: -1 do: [ :i |
		 cls at: copiedValues size - i + 1 put: (copiedValues at: i) ]