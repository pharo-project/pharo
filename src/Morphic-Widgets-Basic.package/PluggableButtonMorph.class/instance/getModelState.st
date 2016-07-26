accessing
getModelState
	"Answer the result of sending the receiver's model the getStateSelector message.
	If the selector expects arguments then supply as for the actionSelector."

	model ifNil: [^ false].
	^getStateSelector ifNil: [false]
		ifNotNil: [getStateSelector numArgs = 0
					ifTrue: [model perform: getStateSelector]
					ifFalse: [argumentsProvider ifNotNil: [
								arguments := argumentsProvider perform: argumentsSelector].
							model perform: getStateSelector withEnoughArguments: arguments]]