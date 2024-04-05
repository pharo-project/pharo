Extension { #name : 'SycDuplicateClassCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycDuplicateClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemFor: ClyFullBrowserClassContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycDuplicateClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $d meta for: ClyFullBrowserClassContext
]
