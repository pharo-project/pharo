Extension { #name : #ClySwitchFocusToPreviousBrowserPaneCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClySwitchFocusToPreviousBrowserPaneCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: Character arrowLeft asShortcut for: ClyFullBrowserContext
]
