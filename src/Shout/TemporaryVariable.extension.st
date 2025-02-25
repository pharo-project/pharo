Extension { #name : 'TemporaryVariable' }

{ #category : '*Shout' }
TemporaryVariable >> styleNameIn: aVariableNode [

	^ self isDefinedByBlock
		  ifTrue: [
			  aVariableNode isDefinition
				  ifTrue: [ #blockPatternTempVar ]
				  ifFalse: [ #blockTempVar ] ]
		  ifFalse: [
			  aVariableNode isDefinition
				  ifTrue: [ #patternTempVar ]
				  ifFalse: [ #tempVar ] ]
]
