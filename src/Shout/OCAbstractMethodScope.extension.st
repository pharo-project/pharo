Extension { #name : 'OCAbstractMethodScope' }

{ #category : '*Shout' }
OCAbstractMethodScope >> hasBindingThatBeginsWith: aString [
	"check weather there are any temporaries defined that start with aString"
	(copiedVars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	(tempVector hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	(tempVars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].

	^ self outerScope hasBindingThatBeginsWith: aString
]
