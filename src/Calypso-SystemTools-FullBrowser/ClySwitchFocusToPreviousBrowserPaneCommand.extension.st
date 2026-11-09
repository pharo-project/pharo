Extension { #name : 'ClySwitchFocusToPreviousBrowserPaneCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClySwitchFocusToPreviousBrowserPaneCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: Character arrowLeft asShortcut for: ClyFullBrowserContext
]
