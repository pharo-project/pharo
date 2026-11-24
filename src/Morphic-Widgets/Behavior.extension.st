Extension { #name : 'Behavior' }

{ #category : '*Morphic-Widgets' }
Behavior >> systemIconName [

	^self isAbstract
				ifTrue: [ #classAbstract ]
				ifFalse: [ super systemIconName ]
]
