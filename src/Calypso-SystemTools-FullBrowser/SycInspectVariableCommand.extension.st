Extension { #name : 'SycInspectVariableCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycInspectVariableCommand class >> fullBrowserDoubleClickActivation [
	<classAnnotation>

	^(CmdDoubleClickActivation for: ClyFullBrowserVariableContext)
		priority: 10000
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycInspectVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserVariableContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycInspectVariableCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^(CmdShortcutActivation
		by: PharoShortcuts current inspectItShortcut for: ClyFullBrowserVariableContext)
			priority: 100000
]
