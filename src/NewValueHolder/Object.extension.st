Extension { #name : #Object }

{ #category : #'*NewValueHolder' }
Object >> asValueHolder [
	"See NewValueHolder class comment"

	^ NewValueHolder value: self
]

{ #category : #'*NewValueHolder' }
Object >> isValueHolder [

	^ false
]
