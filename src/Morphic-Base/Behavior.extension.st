Extension { #name : 'Behavior' }

{ #category : '*Morphic-Base' }
Behavior >> systemIconName [

	^self isAbstract
				ifTrue: [ #classAbstract ]
				ifFalse: [ super systemIconName ]
]
