protocol
falsePositiveOf: aRule
	
	aRule ifNil: [ ^ {}].
	^ aRule isComposite
		ifTrue: [ aRule leaves flatCollect: [ :rule | self falsePositiveOf: rule ]]
		ifFalse: [falsePositives at: aRule ifAbsent: [{}]]
	
	