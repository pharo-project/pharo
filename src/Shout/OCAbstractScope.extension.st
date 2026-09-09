Extension { #name : 'OCAbstractScope' }

{ #category : '*Shout' }
OCAbstractScope >> hasBindingThatBeginsWith: aString [
	"check weather there are any variables defined that start with aString"
	^ outerScope
		ifNil: [ false ]
		ifNotNil: [ outerScope hasBindingThatBeginsWith: aString ]
]
