"
I am a command to find method in methodView of full browser.
I request user with search dialog using method view query.
So dialog shows all methods from method view panel.

By default I am activated by cmd+f.
"
Class {
	#name : 'ClyFindMethodInBrowserCommand',
	#superclass : 'ClyBrowserCommand',
	#instVars : [
		'method'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Methods',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Methods'
}

{ #category : 'menu' }
ClyFindMethodInBrowserCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $f meta for: ClyFullBrowserMethodContext
]

{ #category : 'execution' }
ClyFindMethodInBrowserCommand >> executeRefactoring [

	self browser selectMethod: method
]

{ #category : 'accessing' }
ClyFindMethodInBrowserCommand >> iconName [
	^#smallFind
]

{ #category : 'testing' }
ClyFindMethodInBrowserCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserMethodContext
]

{ #category : 'accessing' }
ClyFindMethodInBrowserCommand >> menuGroup [

	^ ClyQueryMenuGroup
]

{ #category : 'accessing' }
ClyFindMethodInBrowserCommand >> name [
	^'Find method'
]

{ #category : 'accessing' }
ClyFindMethodInBrowserCommand >> order [

	^ 1000
]

{ #category : 'execution' }
ClyFindMethodInBrowserCommand >> prepareFullExecution [

	super prepareFullExecution.
	method := StBrowserSearchPresenter searchConfiguring: [ :presenter :dialog |
			          dialog title: 'Choose method'.
			          presenter items: (self browser itemsForQuery: self browser methodView query).
			          presenter itemsList display: [ :aMethod | aMethod selector ] ]
]
