"
Go forward in the navigation history

I am a command to go forward in the browser.
I move browser to the next navigation state which was recorded after go back operation.

By default I can be executed by alt+right shortcut
"
Class {
	#name : 'ClyNavigateBrowserForwardCommand',
	#superclass : 'ClyBrowserNavigationCommand',
	#category : 'Calypso-Browser-NavigationHistory',
	#package : 'Calypso-Browser',
	#tag : 'NavigationHistory'
}

{ #category : 'execution' }
ClyNavigateBrowserForwardCommand >> executeRefactoring [
	self browser navigateForward
]

{ #category : 'accessing' }
ClyNavigateBrowserForwardCommand >> iconName [
	^#forward
]

{ #category : 'accessing' }
ClyNavigateBrowserForwardCommand >> name [
	^'go forward'
]
