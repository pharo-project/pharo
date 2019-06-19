Extension { #name : #ClySwitchFocusToNextBrowserPaneCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClySwitchFocusToNextBrowserPaneCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: Character arrowRight asShortcut for: ClyFullBrowserContext
]
