Extension { #name : #Object }

{ #category : #'*NewValueHolder-Core' }
Object >> asValueHolder [
	"See NewValueHolder class comment"

	^ NewValueHolder value: self
]

{ #category : #'*NewValueHolder-Core' }
Object >> isValueHolder [

	^ false
]
