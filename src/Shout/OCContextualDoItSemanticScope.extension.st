Extension { #name : 'OCContextualDoItSemanticScope' }

{ #category : '*Shout' }
OCContextualDoItSemanticScope >> hasBindingThatBeginsWith: aString [

	(targetContext astScope hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].

	^super hasBindingThatBeginsWith: aString
]
