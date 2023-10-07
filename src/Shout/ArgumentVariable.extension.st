Extension { #name : 'ArgumentVariable' }

{ #category : '*Shout' }
ArgumentVariable >> styleNameIn: aRBVariableNode [

	^ self isDefinedByBlock
		  ifTrue: [
			  aRBVariableNode isDefinition
				  ifTrue: [ #blockPatternArg ]
				  ifFalse: [ #blockArg ] ]
		  ifFalse: [
			  aRBVariableNode isDefinition			
				  ifTrue: [ #patternArg ]
				  ifFalse: [ #argument ] ]
]
