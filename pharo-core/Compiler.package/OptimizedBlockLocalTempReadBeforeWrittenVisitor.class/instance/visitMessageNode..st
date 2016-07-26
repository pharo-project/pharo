visiting
visitMessageNode: aMessageNode
	| savedWritten writtenPostFirstArm |
	(aMessageNode isOptimized
	 and: [#(ifTrue:ifFalse: ifFalse:ifTrue: ifNil:ifNotNil: ifNotNil:ifNil:) includes: aMessageNode selector key]) ifFalse:
		[^super visitMessageNode: aMessageNode].
	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	savedWritten := written copy.
	aMessageNode argumentsInEvaluationOrder
		do: [:argument|
			argument isBlockNode
				ifTrue: [| savedIOB |
					savedIOB := inOptimizedBlock.
					inOptimizedBlock := true.
					[argument accept: self]
						ensure: [inOptimizedBlock := savedIOB]]
				ifFalse: [argument accept: self]]
		separatedBy:
			[writtenPostFirstArm := written.
			 written := savedWritten].
	(written notNil
	 and: [writtenPostFirstArm notNil]) ifTrue:
		[written := written intersection: writtenPostFirstArm]