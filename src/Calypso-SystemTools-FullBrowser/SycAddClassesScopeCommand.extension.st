Extension { #name : 'SycAddClassesScopeCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAddClassesScopeCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext
]
