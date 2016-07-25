printing
printOn: aStream indent: level
	"may not need this check anymore - may be fixed by the #receiver: change"
	special ifNil: [^aStream nextPutAll: '** MessageNode with nil special **'].

	special > 0 ifTrue:
		[^self perform: self macroPrinter with: aStream with: level].

	self printReceiver: receiver on: aStream indent: level.
	selector isForFFICall
		ifTrue:
			[aStream space.
			 selector
				printAsFFICallWithArguments: arguments
				on: aStream
				indent: 0]
		ifFalse:
			[self printKeywords: selector key
				 arguments: arguments
				 on: aStream
				 indent: level]