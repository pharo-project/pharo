Extension { #name : 'ClassVariable' }

{ #category : '*Shout' }
ClassVariable >> styleNameIn: aRBVariableNode [

	^ self isPoolVariable
		  ifTrue: [ #poolConstant ]
		  ifFalse: [ #classVar ]
]
