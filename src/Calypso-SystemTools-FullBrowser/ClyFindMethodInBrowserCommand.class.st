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
ClyFindMethodInBrowserCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserMethodContext
]

{ #category : 'menu' }
ClyFindMethodInBrowserCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $f meta for: ClyFullBrowserMethodContext
]

{ #category : 'accessing' }
ClyFindMethodInBrowserCommand >> defaultMenuIconName [
	^#smallFind
]

{ #category : 'accessing' }
ClyFindMethodInBrowserCommand >> defaultMenuItemName [
	^'Find method'
]

{ #category : 'execution' }
ClyFindMethodInBrowserCommand >> execute [

	browser selectMethod: method
]

{ #category : 'execution' }
ClyFindMethodInBrowserCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	method := StBrowserSearchPresenter searchConfiguring: [ :presenter :dialog |
		          dialog title: 'Choose method'.
		          presenter items: (aToolContext browser itemsForQuery: browser methodView query).
		          presenter itemsList display: [ :aMethod | aMethod selector ] ]
]
