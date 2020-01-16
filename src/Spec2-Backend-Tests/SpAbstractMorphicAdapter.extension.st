Extension { #name : #SpAbstractMorphicAdapter }

{ #category : #'*Spec2-Backend-Tests' }
SpAbstractMorphicAdapter >> typeForSearch: aString [
	"emulate we are typing to perform a search"
	
	self type: aString
]
