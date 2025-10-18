Extension { #name : 'SycAddClassesScopeCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAddClassesScopeCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ SycScopesMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext
]
