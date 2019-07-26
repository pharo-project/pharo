Extension { #name : #Set }

{ #category : #'*Monticello-Storing' }
Set >> comeFullyUpOnReload: smartRefStream [
	"Symbols have new hashes in this image."

	^self 
		rehash; 
		yourself

]
