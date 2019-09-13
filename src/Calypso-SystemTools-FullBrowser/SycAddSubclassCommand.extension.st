Extension { #name : #SycAddSubclassCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddSubclassCommand class >> fullBrowserTabActivation [
	<classAnnotation>
	
	^ClyBrowserTabCommandActivation for: ClyClassContextOfFullBrowser 
]
