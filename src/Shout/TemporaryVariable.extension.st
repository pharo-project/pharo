Extension { #name : 'TemporaryVariable' }

{ #category : '*Shout' }
TemporaryVariable >> styleNameIn: aRBVariableNode [

	^ self isDefinedByBlock
		  ifTrue: [
			  aRBVariableNode isDefinition
				  ifTrue: [ #blockPatternTempVar ]
				  ifFalse: [ #blockTempVar ] ]
		  ifFalse: [
			  aRBVariableNode isDefinition
				  ifTrue: [ #patternTempVar ]
				  ifFalse: [ #tempVar ] ]
]
