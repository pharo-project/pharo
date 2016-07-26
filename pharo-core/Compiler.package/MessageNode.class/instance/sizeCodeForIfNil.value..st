code generation
sizeCodeForIfNil: encoder value: forValue

	| theNode theSize theSelector |
	equalNode := encoder encodeSelector: #==.
	sizes := Array new: 1.
	theNode := arguments first.
	theSelector := #ifNotNil:.
	forValue
		ifTrue:
			[sizes at: 1 put: (theSize := (encoder sizePop + (theNode sizeCodeForEvaluatedValue: encoder))).
			 ^(receiver sizeCodeForValue: encoder)
			 + encoder sizeDup
			 + (encoder sizePushSpecialLiteral: nil)
			 + (equalNode sizeCode: encoder args: 1 super: false)
			 + (self 
					sizeCode: encoder forBranchOn: selector key == theSelector 
					dist: theSize)
			 + theSize]
		ifFalse:
			[sizes at: 1 put: (theSize := (theNode sizeCodeForEvaluatedEffect: encoder)).
			 ^(receiver sizeCodeForValue: encoder)
				+ (encoder sizePushSpecialLiteral: nil)
				+ (equalNode sizeCode: encoder args: 1 super: false)
				+ (self 
					sizeCode: encoder
					forBranchOn: selector key == theSelector 
					dist: theSize)
				+ theSize]