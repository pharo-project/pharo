"
I am a command to move browser focus to previous (left) navigation pane
"
Class {
	#name : 'ClySwitchFocusToPreviousBrowserPaneCommand',
	#superclass : 'ClyBrowserCommand',
	#category : 'Calypso-Browser-Commands',
	#package : 'Calypso-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClySwitchFocusToPreviousBrowserPaneCommand >> canBeExecuted [

	^ self context browser isNavigationPanelFocused
]

{ #category : 'execution' }
ClySwitchFocusToPreviousBrowserPaneCommand >> executeRefactoring [
	self browser switchFocusToPreviousPane
]
