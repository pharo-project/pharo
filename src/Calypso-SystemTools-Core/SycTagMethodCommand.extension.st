Extension { #name : #SycTagMethodCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycTagMethodCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyMethod asCalypsoItemContext 
]
