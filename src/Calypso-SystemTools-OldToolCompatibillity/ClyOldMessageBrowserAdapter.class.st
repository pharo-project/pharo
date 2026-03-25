"
I am a special adapter for the old message browser interface which users call using: 

	Smalltalk tools messageList 

The most of system queries are triggered by SystemNavigation which collects list of result methods and pass them to the registered message browser.
In contrast Calypso needs instance of query.

So to suppord old (current) approach I build special ClyOldMessageBrowserQuery on constant list of methods and then I open ClyQueryBrowser on it.
 
When Calypso is registered as default browser I am used as #messageList.
In future we should introduce #queryBrowser instead of it with direct usage of Calypso queries.

Internal Representation and Key Implementation Points.

    Instance Variables
	autoSelect:		<Boolean>
	messages:		<Collection of<CompiledMethod>>
	refreshingBlock:		<BlockClosure>
	title:		<String>
"
Class {
	#name : 'ClyOldMessageBrowserAdapter',
	#superclass : 'Object',
	#instVars : [
		'title',
		'autoSelect',
		'refreshingBlock',
		'openedBrowser',
		'methods'
	],
	#category : 'Calypso-SystemTools-OldToolCompatibillity',
	#package : 'Calypso-SystemTools-OldToolCompatibillity'
}

{ #category : 'opening' }
ClyOldMessageBrowserAdapter class >> browse: methods [

	^ self new
		  methods: methods;
		  open
]

{ #category : 'opening' }
ClyOldMessageBrowserAdapter class >> browse: methods title: aString [
	^ self new
		methods: methods;
		title: aString;
		open
]

{ #category : 'opening' }
ClyOldMessageBrowserAdapter class >> browse: methods title: aString autoSelect: aSelectString [
	^ self new
		methods: methods;
		title: aString;
		autoSelect: aSelectString;
		open
]

{ #category : 'opening' }
ClyOldMessageBrowserAdapter class >> browseClasses: classes [
	^ClyQueryBrowserMorph browseClasses: classes
]

{ #category : 'opening' }
ClyOldMessageBrowserAdapter class >> browseImplementorsOf: aSymbol inNameResolver: methodContext [

	^ ClyQueryBrowserMorph
		  browseImplementorsOf: aSymbol
		  inNameResolver: methodContext
]

{ #category : 'opening' }
ClyOldMessageBrowserAdapter class >> browseSendersOf: aSymbol [
	^ClyQueryBrowserMorph browseSendersOf: aSymbol
]

{ #category : 'instance creation' }
ClyOldMessageBrowserAdapter class >> on: aNavigationEnvironment systemScope: aSystemEnvironmentScope [ 
	
	^ ClyQueryBrowserMorph
		on: aNavigationEnvironment 
		systemScope: aSystemEnvironmentScope
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> autoSelect [
	^ autoSelect
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> autoSelect: anObject [
	autoSelect := anObject
]

{ #category : 'opening' }
ClyOldMessageBrowserAdapter >> close [
	openedBrowser ifNotNil: #close
]

{ #category : 'initialization' }
ClyOldMessageBrowserAdapter >> initialize [
	super initialize.

	title := 'MessageBrowser'
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> methods [
	^ methods
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> methods: anObject [
	methods := anObject
]

{ #category : 'opening' }
ClyOldMessageBrowserAdapter >> open [
	| query compiledMethods comments |
	compiledMethods := methods
		select: [:each | (each isRingObject and: [ each isMethod ]) or: [ each isCompiledMethod  ] ]
		thenCollect: [ :each | each compiledMethod ].
	comments := methods
		select: [:each | each isRingObject and: [ each isComment  ]]
		thenCollect: [ :each | ClyClassComment of: each realParent ].
	query := ClyOldMessageBrowserQuery named: title with: compiledMethods asOrderedCollection, comments.
	query
		criteriaString: autoSelect;
		criteriaBlock: refreshingBlock.
	openedBrowser := ClyQueryBrowserMorph openOn: query
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> refreshingBlock [
	^ refreshingBlock
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> refreshingBlock: anObject [
	refreshingBlock := anObject
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> title [
	^ title
]

{ #category : 'accessing' }
ClyOldMessageBrowserAdapter >> title: anObject [
	title := anObject
]
