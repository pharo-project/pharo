actions
evaluateSupplyingAnswers: loadBlock

	| answers |
	(answers := self spec answers) notEmpty
		ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
		ifFalse: [ loadBlock value]