Extension { #name : #ClyAddClassesScopeCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyAddClassesScopeCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext  
]
