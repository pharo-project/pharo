Extension { #name : #Gofer }

{ #category : #'*metacello-testsmc' }
Gofer >> metacelloCleanup [
	"Cleans the specified packages."
	
	^ self execute: MetacelloTestsGoferCleanup
]

{ #category : #'*metacello-testsmc' }
Gofer >> metacelloLoad [
	"Load the specified packages into the image."
	
	^ self execute: MetacelloGoferLoad
]

{ #category : #'*metacello-testsmc' }
Gofer >> metacelloUnload [
	"Unload the specified packages."
	
	^ self execute: MetacelloTestsGoferUnload
]
