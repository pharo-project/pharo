accessing - values
at: key ifPresent: oneArgBlock ifAbsent: absentBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the oneArgBlock with the value associated with the key, otherwise answer the value of absentBlock."
	
	self at: key ifPresent: [ :v | ^oneArgBlock cull: v ].
	^absentBlock value