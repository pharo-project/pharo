Extension { #name : #SycShowMethodVersionCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycShowMethodVersionCommand class >> methodBrowserTabActivation [
	<classAnnotation>
	
	^ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext 
]

{ #category : #'*Calypso-SystemTools-Core' }
SycShowMethodVersionCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyMethod asCalypsoItemContext 
]
