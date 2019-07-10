Extension { #name : #SycMethodRepackagingCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycMethodRepackagingCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyMethod asCalypsoItemContext 
]
