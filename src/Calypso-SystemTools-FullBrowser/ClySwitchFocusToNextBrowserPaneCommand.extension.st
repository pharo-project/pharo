Extension { #name : 'ClySwitchFocusToNextBrowserPaneCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClySwitchFocusToNextBrowserPaneCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: Character arrowRight asShortcut for: ClyFullBrowserContext
]
