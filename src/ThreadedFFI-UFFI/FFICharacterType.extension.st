Extension { #name : #FFICharacterType }

{ #category : #'*ThreadedFFI-UFFI' }
FFICharacterType >> tfExternalType [
	
	^ TFBasicType schar
]

{ #category : #'*ThreadedFFI-UFFI' }
FFICharacterType >> tfExternalTypeWithArity [
	
	"If it is a char* we can assume it is a String"
	self pointerArity = 1 ifTrue: [ ^TFDerivedType string ]. 

	^ super tfExternalTypeWithArity
]
