"
I am a command to move browser focus to previous (left) navigation pane
"
Class {
	#name : #ClySwitchFocusToPreviousBrowserPaneCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-Browser-Commands'
}

{ #category : #testing }
ClySwitchFocusToPreviousBrowserPaneCommand class >> canBeExecutedInContext: aBrowserContext [
	^aBrowserContext browser isNavigationPanelFocused
]

{ #category : #execution }
ClySwitchFocusToPreviousBrowserPaneCommand >> execute [
	browser switchFocusToPreviousPane
]
