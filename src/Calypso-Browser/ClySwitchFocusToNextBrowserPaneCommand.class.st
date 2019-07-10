"
I am a command to move browser focus to next (right) navigation pane
"
Class {
	#name : #ClySwitchFocusToNextBrowserPaneCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-Browser-Commands'
}

{ #category : #testing }
ClySwitchFocusToNextBrowserPaneCommand class >> canBeExecutedInContext: aBrowserContext [
	^aBrowserContext browser isNavigationPanelFocused
]

{ #category : #execution }
ClySwitchFocusToNextBrowserPaneCommand >> execute [
	browser switchFocusToNextPane
]
