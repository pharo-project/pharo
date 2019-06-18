"
I am a command to go back in the browser.
I move browser to previous navigation state.

By default I can be executed by alt+left shortcut
"
Class {
	#name : #ClyNavigateBrowserBackCommand,
	#superclass : #ClyBrowserNavigationCommand,
	#category : #'Calypso-Browser-NavigationHistory'
}

{ #category : #accessing }
ClyNavigateBrowserBackCommand class >> defaultShortcut [
	^Character arrowLeft alt
]

{ #category : #accessing }
ClyNavigateBrowserBackCommand >> defaultMenuIconName [
	^#back
]

{ #category : #accessing }
ClyNavigateBrowserBackCommand >> defaultMenuItemName [ 
	^'go backward'
]

{ #category : #accessing }
ClyNavigateBrowserBackCommand >> description [
	^'go backward in the navigation history'
]

{ #category : #execution }
ClyNavigateBrowserBackCommand >> execute [
	browser navigateBack
]
