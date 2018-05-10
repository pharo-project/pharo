Extension { #name : #Object }

{ #category : #'*System-Support' }
Object >> isPrimitiveError [
	"Answer a boolean indicating if the receiver is an error object returned by a primitive"
	^false
]

{ #category : #'*System-Support' }
Object class >> registerToolsOn: aToolRegistry [

	" Override to register any tools for Smalltalk tools registry. " 
]

{ #category : #'*System-Support' }
Object >> systemNavigation [

	^ SystemNavigation new
]
