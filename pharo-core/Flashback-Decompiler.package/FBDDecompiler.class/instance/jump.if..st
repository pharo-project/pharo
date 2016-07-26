control flow instructions
jump: distance if: boolean
	"If we reach this code, this means this is a condition (if) and not a loop."

	| expr trueSeq falseSeq pastSequence hasPopped |
	pastSequence := currentSequence.
	expr := simulatedStack removeLast.
	trueSeq := builder codeEmptySequence.
	falseSeq := builder codeEmptySequence.
	hasPopped := boolean
		ifTrue: [ self interpret: falseSeq then: trueSeq distance: distance ]
		ifFalse: [ self interpret: trueSeq then: falseSeq distance: distance ].
	currentSequence := pastSequence.
	simulatedStack addLast: (builder codeConditionalNode: expr trueSequence: trueSeq falseSequence: falseSeq).
	hasPopped
		ifFalse: [ self doPop ]