Extension { #name : #Number }

{ #category : #'*Morphic-Base' }
Number >> defaultLabel [
	"Answer the default label to be used for an Inspector window on the receiver."

	^ super defaultLabel, ': ', self printString
]
