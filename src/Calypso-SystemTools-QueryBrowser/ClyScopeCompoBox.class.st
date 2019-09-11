"
I am a tool to switch query browser to new choosen scope.
I use compobox to allow user choose it.

I am shown as first item in the browser toolbar.

By instances should be created on browser: 

	ClyScopeCompoBox for: ClyQueryBrowser.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyQueryBrowser>
	dropList:		<Object>

"
Class {
	#name : #ClyScopeCompoBox,
	#superclass : #ClyBrowserToolbarItemMorph,
	#instVars : [
		'dropList',
		'browser'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-UI'
}

{ #category : #'instance creation' }
ClyScopeCompoBox class >> for: aBrowser [
	^self new 
		browser: aBrowser
]

{ #category : #accessing }
ClyScopeCompoBox >> browser [
	^ browser
]

{ #category : #accessing }
ClyScopeCompoBox >> browser: aMethodBrowser [
	browser := aMethodBrowser.
	self build
]

{ #category : #initialization }
ClyScopeCompoBox >> build [
	dropList := self theme 
		newDropListIn: World
		for: self 
		list: #scopeNames
		getSelected: #currentScopeIndex 
		setSelected: #currentScopeIndex: 
		getEnabled: nil useIndex: true
		help: self currentScope description. 
		
	self addMorph: dropList
]

{ #category : #accessing }
ClyScopeCompoBox >> currentScope [
	^ self scopes detect: [ :each | browser isQueryScopeActive: each]
]

{ #category : #accessing }
ClyScopeCompoBox >> currentScope: aScope [

	browser switchScopeTo: aScope.	
	dropList setBalloonText: aScope description. 
]

{ #category : #accessing }
ClyScopeCompoBox >> currentScopeIndex [
	^self scopes indexOf: self currentScope
]

{ #category : #accessing }
ClyScopeCompoBox >> currentScopeIndex: index [
	self currentScope: (self scopes at: index)
]

{ #category : #initialization }
ClyScopeCompoBox >> initialize [
	super initialize.
	self 
		changeTableLayout;
		hResizing: #rigid;
		width: 200
]

{ #category : #accessing }
ClyScopeCompoBox >> scopeNames [

	^self scopes collect: [ :each | each description capitalized]
]

{ #category : #accessing }
ClyScopeCompoBox >> scopes [

	^browser allNavigationScopes 
]
