Extension { #name : 'SycOpenClassMenuCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycOpenClassMenuCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10100
		for: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycOpenClassMenuCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $t meta for: ClyClass asCalypsoItemContext
]
