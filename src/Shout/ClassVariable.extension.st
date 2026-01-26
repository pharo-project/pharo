Extension { #name : 'ClassVariable' }

{ #category : '*Shout' }
ClassVariable >> styleNameIn: aVariableNode [

	^ self isPoolVariable
		  ifTrue: [ #poolConstant ]
		  ifFalse: [ #classVar ]
]
