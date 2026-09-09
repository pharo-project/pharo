Extension { #name : 'ArgumentVariable' }

{ #category : '*Shout' }
ArgumentVariable >> styleNameIn: aVariableNode [

	^ self isDefinedByBlock
		  ifTrue: [
			  aVariableNode isDefinition
				  ifTrue: [ #blockPatternArg ]
				  ifFalse: [ #blockArg ] ]
		  ifFalse: [
			  aVariableNode isDefinition			
				  ifTrue: [ #patternArg ]
				  ifFalse: [ #argument ] ]
]
