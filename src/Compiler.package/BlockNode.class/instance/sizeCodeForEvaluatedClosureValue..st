code generation (closures)
sizeCodeForEvaluatedClosureValue: encoder
	"The closure value primitives push the arguments and the copied values.
	 The compiler guarantees that any copied values come before all local temps.
	 So on closure activation we only need to push nils for the remaining temporaries."
	^temporaries size * (NodeNil sizeCodeForValue: encoder)
	+ (self
		reindexingLocalsDo: [self sizeCodeForEvaluatedValue: encoder]
		encoder: nil "don't store temps yet")
	+ (self returns ifTrue: [0] ifFalse: [encoder sizeReturnTopToCaller])