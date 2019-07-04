"
I am a command to go forward in the browser.
I move browser to the next navigation state which was recorded after go back operation.

By default I can be executed by alt+right shortcut
"
Class {
	#name : #ClyNavigateBrowserForwardCommand,
	#superclass : #ClyBrowserNavigationCommand,
	#category : #'Calypso-Browser-NavigationHistory'
}

{ #category : #accessing }
ClyNavigateBrowserForwardCommand class >> defaultShortcut [
	^Character arrowRight alt
]

{ #category : #accessing }
ClyNavigateBrowserForwardCommand >> defaultMenuIconName [
	^#forward
]

{ #category : #accessing }
ClyNavigateBrowserForwardCommand >> defaultMenuItemName [ 
	^'go forward'
]

{ #category : #accessing }
ClyNavigateBrowserForwardCommand >> description [
	^'go forward in the navigation history'
]

{ #category : #execution }
ClyNavigateBrowserForwardCommand >> execute [
	browser navigateForward
]
