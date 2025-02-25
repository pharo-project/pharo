Extension { #name : 'Object' }

{ #category : '*System-Support' }
Object >> isPrimitiveError [
	"Answer a boolean indicating if the receiver is an error object returned by a primitive"
	^false
]

{ #category : '*System-Support' }
Object >> systemNavigation [

	^ SystemNavigation new
]
