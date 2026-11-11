Extension { #name : 'SycOpenClassMenuCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycOpenClassMenuCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^ClyContextMenuActivation byRootGroupItemOrder: 10100
		for: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycOpenClassMenuCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $t meta for: ClyClass asCalypsoItemContext
]
