Extension { #name : 'OCSemanticScope' }

{ #category : '*Shout' }
OCSemanticScope >> hasBindingThatBeginsWith: aString [

	(self targetClass hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].

	^super hasBindingThatBeginsWith: aString
]
