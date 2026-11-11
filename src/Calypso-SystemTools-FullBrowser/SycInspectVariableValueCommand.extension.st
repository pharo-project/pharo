Extension { #name : 'SycInspectVariableValueCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycInspectVariableValueCommand class >> fullBrowserDoubleClickActivation [
	<classAnnotation>

	^(ClyDoubleClickActivation for: ClyFullBrowserVariableContext)
		priority: 10000
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycInspectVariableValueCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^(ClyShortcutActivation
		by: PharoShortcuts current inspectItShortcut for: ClyFullBrowserVariableContext)
			priority: 100000
]
