evaluating
valueSupplyingAnswer: anObject
	^ (anObject isCollection and: [anObject isString not])
		ifTrue: [self valueSupplyingAnswers: {anObject}]
		ifFalse: [self valueSupplyingAnswers: {{'*'. anObject}}]