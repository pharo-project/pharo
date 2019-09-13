Extension { #name : #SycChangeMessageSignatureCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycChangeMessageSignatureCommand class >> browserMenuOrder [
	<classAnnotationDependency>
	^2.1
]

{ #category : #'*Calypso-SystemTools-Core' }
SycChangeMessageSignatureCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.3 for: ClyMethodSourceCodeContext
]
