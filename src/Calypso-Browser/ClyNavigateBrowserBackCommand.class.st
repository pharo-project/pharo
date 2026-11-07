"
I am a command to go back in the browser.
I move browser to previous navigation state.

By default I can be executed by alt+left shortcut
"
Class {
	#name : 'ClyNavigateBrowserBackCommand',
	#superclass : 'ClyBrowserNavigationCommand',
	#category : 'Calypso-Browser-NavigationHistory',
	#package : 'Calypso-Browser',
	#tag : 'NavigationHistory'
}

{ #category : 'accessing' }
ClyNavigateBrowserBackCommand class >> defaultShortcut [
	^Character arrowLeft alt
]

{ #category : 'accessing' }
ClyNavigateBrowserBackCommand >> description [
	^'go backward in the navigation history'
]

{ #category : 'execution' }
ClyNavigateBrowserBackCommand >> executeRefactoring [
	self browser navigateBack
]

{ #category : 'accessing' }
ClyNavigateBrowserBackCommand >> iconName [
	^#back
]

{ #category : 'accessing' }
ClyNavigateBrowserBackCommand >> name [
	^'go backward'
]
