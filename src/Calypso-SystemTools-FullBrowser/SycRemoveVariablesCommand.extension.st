Extension { #name : 'SycRemoveVariablesCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRemoveVariablesCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserVariableContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRemoveVariablesCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ CmdShortcutActivation removalFor: ClyFullBrowserVariableContext
]
