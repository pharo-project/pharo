Extension { #name : #Gofer }

{ #category : #'*Metacello-TestsMC' }
Gofer >> metacelloLoad [
	"Load the specified packages into the image."
	
	^ self execute: MetacelloGoferLoad
]

{ #category : #'*Metacello-TestsMC' }
Gofer >> metacelloUnload [
	"Unload the specified packages."
	
	^ self execute: MetacelloTestsGoferUnload
]
