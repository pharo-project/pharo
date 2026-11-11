Extension { #name : 'SycRemoveVariablesCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRemoveVariablesCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ ClyShortcutActivation removalFor: ClyFullBrowserVariableContext
]
