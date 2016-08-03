protocol
falsePositiveOf: aRule
	
	aRule ifNil: [ ^ {}].
	^ aRule isComposite
		ifTrue: [ aRule rules flatCollect: [ :rule | self falsePositiveOf: rule ]]
		ifFalse: [falsePositives at: aRule ifAbsent: [{}]]
	
	