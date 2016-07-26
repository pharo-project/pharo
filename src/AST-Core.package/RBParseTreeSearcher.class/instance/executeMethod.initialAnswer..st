accessing
executeMethod: aParseTree initialAnswer: anObject 
	answer := anObject.
	searches detect: [:each | (each performOn: aParseTree) notNil] ifNone: [].
	^answer