instruction decoding
popIntoTemporaryVariable: offset
	| maybeTVTag tempVector start |
	maybeTVTag := stack last.
	((maybeTVTag isMemberOf: Association)
	 and: [maybeTVTag key == #pushNewArray]) ifTrue:
		[blockStartsToTempVars notNil "implies we were intialized with temp names."
			ifTrue: "Use the provided temps"
				[[(tempVector := tempVars at: offset + 1 ifAbsent: [ParseNode basicNew]) isTemp
							 and: [tempVector isIndirectTempVector
							 and: [tempVector remoteTemps size = maybeTVTag value size]]] assert]
			ifFalse: "Synthesize some remote temps"
				[tempVector := maybeTVTag value.
				 offset + 1 <= tempVars size
					ifTrue:
						[start := 2.
						 tempVector at: 1 put: (tempVars at: offset + 1)]
					ifFalse:
						[tempVars := (Array new: offset + 1)
										replaceFrom: 1
										to: tempVars size
										with: tempVars.
						start := 1].
				 start to: tempVector size do:
					[:i|
					tempVector
						at: i
						put: (constructor
								codeTemp: numLocalTemps + offset + i - 1
								named: 't', (tempVarCount + i) printString)].
				tempVars at: offset + 1 put: (constructor codeRemoteTemp: offset + 1 remoteTemps: tempVector)].
		 tempVarCount := tempVarCount + maybeTVTag value size.
		 stack removeLast.
		 ^self].
	self pushTemporaryVariable: offset; doStore: statements