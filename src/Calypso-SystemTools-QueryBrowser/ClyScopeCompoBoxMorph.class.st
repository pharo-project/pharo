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
	#name : #ClyScopeCompoBoxMorph,
	#superclass : #ClyBrowserToolbarItemMorph,
	#instVars : [
		'dropList',
		'browser'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-UI'
}

{ #category : #'instance creation' }
ClyScopeCompoBoxMorph class >> for: aBrowser [
	^self new 
		browser: aBrowser
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> browser [
	^ browser
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> browser: aMethodBrowser [
	browser := aMethodBrowser.
	self build
]

{ #category : #initialization }
ClyScopeCompoBoxMorph >> build [
	dropList := self theme 
		newDropListIn: self
		for: self 
		list: #scopeNames
		getSelected: #currentScopeIndex 
		setSelected: #currentScopeIndex: 
		getEnabled: nil useIndex: true
		help: self currentScope description. 
		
	self addMorph: dropList
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> currentScope [
	^ self scopes detect: [ :each | browser isQueryScopeActive: each]
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> currentScope: aScope [

	browser switchScopeTo: aScope.	
	dropList setBalloonText: aScope description. 
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> currentScopeIndex [
	^self scopes indexOf: self currentScope
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> currentScopeIndex: index [
	self currentScope: (self scopes at: index)
]

{ #category : #initialization }
ClyScopeCompoBoxMorph >> initialize [
	super initialize.
	self 
		changeTableLayout;
		hResizing: #rigid;
		width: 200
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> scopeNames [

	^self scopes collect: [ :each | each description capitalized]
]

{ #category : #accessing }
ClyScopeCompoBoxMorph >> scopes [

	^browser allNavigationScopes 
]
