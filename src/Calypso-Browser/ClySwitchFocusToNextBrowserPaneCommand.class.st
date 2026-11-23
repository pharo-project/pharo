"
I am a command to move browser focus to next (right) navigation pane
"
Class {
	#name : 'ClySwitchFocusToNextBrowserPaneCommand',
	#superclass : 'ClyBrowserCommand',
	#category : 'Calypso-Browser-Commands',
	#package : 'Calypso-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClySwitchFocusToNextBrowserPaneCommand >> canBeExecuted [

	^ self context browser isNavigationPanelFocused
]

{ #category : 'execution' }
ClySwitchFocusToNextBrowserPaneCommand >> executeRefactoring [
	self browser switchFocusToNextPane
]
