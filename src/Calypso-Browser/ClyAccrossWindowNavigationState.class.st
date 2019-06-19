"
I represent the state of the browser which spawn another browser.
I am recorded as first item in navigation history of spawned browser.
When I am applied to the browser I close it and activate original one using window reference. If window is closed I recreate new browser instance from scratch.
I keep all original browser parameters in my variables including its own navigation history. So recreated browser instances is ready to continue go back in history. 

Notice that I do not keep reference to original browser. I keep reference to window. When window is closing the browser cleans references to it. So closed window do not reference browser.

To create my instance use following expression: 

	ClyAccrossWindowNavigationState from: aBrowser 
	
Internal Representation and Key Implementation Points.

    Instance Variables
	browserPlugins:		<Collection of<ClyBrowserPlugin>>
	browserState:		<ClyBrowserState>
	navigationEnvironment:		<ClyNavigationEnvironment>
	navigationHistory:		<ClyNavigationHistory>
	window:		<SystemWindow>
	windowGroup:		<ClyGroupWindowMorph>
"
Class {
	#name : #ClyAccrossWindowNavigationState,
	#superclass : #Object,
	#instVars : [
		'windowGroup',
		'window',
		'browserState',
		'browserPlugins',
		'navigationEnvironment',
		'navigationHistory'
	],
	#category : #'Calypso-Browser-NavigationHistory'
}

{ #category : #'instance creation' }
ClyAccrossWindowNavigationState class >> from: aBrowser [
	^self new 
		retrieveStateFrom: aBrowser
]

{ #category : #operations }
ClyAccrossWindowNavigationState >> applyTo: aBrowser byUndo: undoExecuting [

	(window isInWorld and: [(browserState isCurrentStateOf: window model) not]) 
		ifTrue: [ ^self ].
	
	navigationHistory recordAcrossWindowNavigationFrom: aBrowser byUndo: undoExecuting. 
	undoExecuting ifTrue: [ aBrowser close].	
		
	self restoreBrowserWindow.
	window model focusActiveTab 
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> browserPlugins [
	^ browserPlugins
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> browserPlugins: anObject [
	browserPlugins := anObject
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> browserState [
	^ browserState
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> browserState: anObject [
	browserState := anObject
]

{ #category : #private }
ClyAccrossWindowNavigationState >> createBrowser [
	| newBrowser |
	newBrowser := browserState createBrowser.
	
	browserPlugins do: [:each | newBrowser addPlugin: each copy ].
	newBrowser navigationEnvironment: navigationEnvironment.
	newBrowser navigationHistory: navigationHistory.
	browserState restoreStateOf: newBrowser.
	
	^newBrowser
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> navigationEnvironment [
	^ navigationEnvironment
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> navigationEnvironment: anObject [
	navigationEnvironment := anObject
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> navigationHistory [
	^ navigationHistory
]

{ #category : #accessing }
ClyAccrossWindowNavigationState >> navigationHistory: anObject [
	navigationHistory := anObject
]

{ #category : #testing }
ClyAccrossWindowNavigationState >> representsConcreteQuery [
	^true
]

{ #category : #private }
ClyAccrossWindowNavigationState >> restoreBrowserWindow [
	| newBrowser |
	window isInWorld ifFalse: [ 
		"Closed browser window is always cleaned from browser. Look at ClyBrowser>>windowIsClosing"
		newBrowser := self createBrowser.
		window addMorph: newBrowser frame: (0@0 extent: 1@1).
		window model: newBrowser.
		browserState applyTo: newBrowser].
	windowGroup 
		ifNil: [
			window isInWorld 
				ifTrue: [ window comeToFront; activate]
			 	ifFalse: [window openAsIs ]]
		ifNotNil: [ 
			windowGroup isInWorld 
				ifTrue: [ windowGroup window comeToFront ]
				ifFalse: [ windowGroup window openAsIs ].
			windowGroup activateWindow: window]
]

{ #category : #operations }
ClyAccrossWindowNavigationState >> retrieveStateFrom: aBrowser [

	browserState := aBrowser snapshotState.
	(window := aBrowser window) ifNotNil: [
		windowGroup := window ownerThatIsA: GroupWindowMorph].
	browserPlugins := aBrowser plugins collect: [:each | each copy browser: nil].
	navigationEnvironment := aBrowser navigationEnvironment.
	navigationHistory := aBrowser navigationHistory
]
