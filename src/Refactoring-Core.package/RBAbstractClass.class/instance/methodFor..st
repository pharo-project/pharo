method accessing
methodFor: aSelector 
	^self newMethods at: aSelector
		ifAbsent: 
			[| compiledMethod class |
			(self hasRemoved: aSelector) ifTrue: [^nil].
			class := self realClass.
			class isNil ifTrue: [^nil].
			compiledMethod := class compiledMethodAt: aSelector ifAbsent: [nil].
			compiledMethod isNil 
				ifTrue: [nil]
				ifFalse: 
					[RBClassModelFactory rbMethod 
						for: self
						fromMethod: compiledMethod
						andSelector: aSelector]]