"
I am a root of subclasses which represent the state of particular kind of browser.
I am used as navigation history items.

While subclasses can provide additional state they all include following properties: 
- viewStates, the collection of state of all navigation views, instances of ClyQueryViewState.
- selectedTabs, the colletion of selected browser tabs 
- systemScope, the system scope of the browser 

My instances are created with browser: 

	ClyBrowserState of: aBrowser

Or simply ask 
	
	aBrowser snapshotState

During initialization I retrieve the browser state in method #retrieveStateFrom:.

When history performs undo/redo operation it just applies particular browser state to the browser: 

	aBrowserState applyTo: aBrowser byUndo: true
	
Which calls simple #applyTo:. Extra agrument is required to support properly across window navigation.

Navigation history support accors window navigation to be able return back and forward between spawned windows.
It requires browser state to be able create browser instances from scratch because when we are trying to move back to original browser it can be already closed and not exists anymore.
So subclasses should implement #createBrowser method. It should just return new browser instance without any initialization logic. 	
 
Internal Representation and Key Implementation Points.

    Instance Variables
	viewStates:		<Collection of<ClyQueryViewState>>
	selectedTabs:		<Collection of<ClyBrowserTool>>
	systemScope:		<ClySystemScope>

"
Class {
	#name : #ClyBrowserState,
	#superclass : #Object,
	#instVars : [
		'viewStates',
		'selectedTabs',
		'systemScope'
	],
	#category : #'Calypso-Browser-NavigationHistory'
}

{ #category : #'instance creation' }
ClyBrowserState class >> of: aBrowser [
	^self new 
		retrieveStateFrom: aBrowser
]

{ #category : #operations }
ClyBrowserState >> applyTo: aBrowser [

	aBrowser changeStateBy: [ 
		self restoreStateOf: aBrowser.	
		self restoreNavigationStateOf: aBrowser.
		aBrowser tabManager restoreBrowserState: self
	]
]

{ #category : #operations }
ClyBrowserState >> applyTo: aBrowser byUndo: undoExecuting [
	self applyTo: aBrowser
]

{ #category : #private }
ClyBrowserState >> createBrowser [
	self subclassResponsibility 
]

{ #category : #testing }
ClyBrowserState >> isCurrentStateOf: aBrowser [

	aBrowser navigationViews with: viewStates do: [:view :state | 
		(state isCurrentStateOf: view) ifFalse: [ ^false]].
	
	^aBrowser tabManager selectsTools: selectedTabs
]

{ #category : #testing }
ClyBrowserState >> representsConcreteQuery [
	"this method allows to detect that browser is in state when it not shows any concrete content at any of panels. It is happens when browser was not requested to show anything yet"
	
	^(viewStates allSatisfy: [ :each | each representsConcreteQuery not ]) not
]

{ #category : #operations }
ClyBrowserState >> restoreNavigationStateOf: aBrowser [
	
	aBrowser navigationViews with: viewStates do: [:view :state |
		view ignoreNavigationDuring: [ 
			state applyTo: view]]
]

{ #category : #private }
ClyBrowserState >> restoreStateOf: aBrowser [

	aBrowser systemScope: systemScope
]

{ #category : #operations }
ClyBrowserState >> retrieveStateFrom: aBrowser [

	systemScope :=  aBrowser systemScope.
	viewStates := aBrowser navigationViews collect: [ :each | each snapshotState ].
	selectedTabs := aBrowser tabManager selectedTools 
		collect: [ :each | each copyForBrowserStateSnapshot ].
	
]

{ #category : #accessing }
ClyBrowserState >> selectedTabs [
	^ selectedTabs
]

{ #category : #accessing }
ClyBrowserState >> selectedTabs: anObject [
	selectedTabs := anObject
]

{ #category : #accessing }
ClyBrowserState >> systemScope [
	^ systemScope
]

{ #category : #accessing }
ClyBrowserState >> systemScope: anObject [
	systemScope := anObject
]

{ #category : #accessing }
ClyBrowserState >> viewStates [
	^ viewStates
]

{ #category : #accessing }
ClyBrowserState >> viewStates: anObject [
	viewStates := anObject
]
