Extension { #name : 'Process' }

{ #category : '*Reflectivity' }
Process >> findContextsForMethod: aMethod [

	| context priorContext found methodClass methodSelector homeMethod |
	found := OrderedCollection new.
	methodClass := aMethod methodClass.
	methodSelector := aMethod selector.

	context := suspendedContext.
	priorContext := nil.
	[ context isNil ] whileFalse: [
		homeMethod := context homeMethod.
		(homeMethod methodClass == methodClass and: [
			 homeMethod selector == methodSelector ]) ifTrue: [ found add: context ].
		priorContext := context.
		context := context sender ].
	^ found
]
