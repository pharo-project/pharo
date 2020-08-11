"
I am a single object inspector.
I present a notebook with the possible inspection of the object.

I am usually embedded into an StInspector, but I can be used standalone.
"
Class {
	#name : #StObjectInspector,
	#superclass : #StPresenter,
	#instVars : [
		'views',
		'evaluator',
		'model',
		'activationBlock',
		'header',
		'alivePresentersByPage'
	],
	#classVars : [
		'PreferredExtent'
	],
	#classInstVars : [
		'header'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #commands }
StObjectInspector class >> buildCommandsGroupWith: presenter forRoot: aCmCommandsGroup [
	| group |
	group := (CmCommandGroup named: 'Toolbar') asSpecGroup.
	(StInspectorCommand toolbarCommandsFor: presenter)
		do: [ :cmd | group register: cmd ].
	aCmCommandsGroup register: group
]

{ #category : #specs }
StObjectInspector class >> defaultSpec [
	<spec: #default>
	
	^ SpBoxLayout newVertical 
		add: #header expand: false fill: false padding: 5;
		add: (SpPanedLayout newVertical
			position: 70 percent;
			add: #views;
			add: #evaluator;
			yourself);
		yourself
]

{ #category : #accessing }
StObjectInspector class >> defaultTitle [

	^ 'Inspector'
]

{ #category : #'tools registry' }
StObjectInspector class >> inspect: anObject [

	^ self openOn: anObject
]

{ #category : #opening }
StObjectInspector class >> onObject: anObject [

	^ self on: (StInspectorModel on: anObject)
]

{ #category : #opening }
StObjectInspector class >> openOn: anObject [

	^ (self onObject: anObject) openWithSpec
]

{ #category : #private }
StObjectInspector >> activate: aSelection [

	activationBlock ifNil: [ ^ self ].
	activationBlock cull: aSelection
]

{ #category : #private }
StObjectInspector >> addActivationTo: aPresenter [
	
	aPresenter hasOutputActivationPort ifFalse: [ ^ self ]. 
	aPresenter outputActivationPort 
		transmitDo: [ :selection | self activate: selection ]
]

{ #category : #private }
StObjectInspector >> allPages [
	| pages |

	pages := self model objectInspectionPages.
	pages do: [ :each | 
		each whenRetrievedDo: [ :presenter | 
			self keepPresenter: presenter.
			self addActivationTo: presenter ] ].
	
	^ pages
	
]

{ #category : #'private builder' }
StObjectInspector >> buildToolbar [

	^ self rootCommandsGroup asToolbarPresenterWith: [ :presenter | 
			presenter 
				displayMode: self application displayMode;
				addStyle: 'stToolbar' ]
]

{ #category : #transmission }
StObjectInspector >> defaultInputPort [

	^ self inputModelPort
]

{ #category : #'api-focus' }
StObjectInspector >> defaultKeyboardFocus [

	^ views pages 
		ifNotEmpty: [ :pages | pages first activePresenter ] 
		ifEmpty: [ self ]
]

{ #category : #private }
StObjectInspector >> evaluatorInitialText [

	^ '"{1}"
self ' 
	format: { self model smallDescriptionString }
]

{ #category : #initialization }
StObjectInspector >> initialize [

	super initialize.
	"I keep the presenters I'm showing in this variable because they can change (and they are 
	 initialized lazily in the notebook)"
	alivePresentersByPage := WeakSet new	
]

{ #category : #initialization }
StObjectInspector >> initializePresenters [

	header := self instantiate: StHeaderBar.
	views := self newNotebook.	
	evaluator := self newCode
		beForObject: self model inspectedObject;
		yourself.
	evaluator text: self evaluatorInitialText.
	self refresh
]

{ #category : #initialization }
StObjectInspector >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter 
		title: self title;
		toolbar: self buildToolbar
]

{ #category : #transmission }
StObjectInspector >> inputModelPort [

	^ SpModelPort newPresenter: self
]

{ #category : #accessing }
StObjectInspector >> inspectedObject [
 	
	^ self model inspectedObject
]

{ #category : #private }
StObjectInspector >> keepPresenter: aPresenter [

	alivePresentersByPage add: aPresenter
]

{ #category : #'model access' }
StObjectInspector >> model [ 

	^ model
]

{ #category : #'model access' }
StObjectInspector >> model: anInspectorModel [

	"if same object, I do not want to update to not trigger infinite updates"
	model inspectedObject == anInspectorModel inspectedObject 
		ifTrue: [ ^ self ].
	model := anInspectorModel.
	self refresh
]

{ #category : #accessing }
StObjectInspector >> rawInspectorPageOrNil [
	^ views pages detect: [ :page | page title = 'Raw' ] ifNone: [ nil ]
]

{ #category : #'private updating' }
StObjectInspector >> refresh [
	"model changed?  
	 update inspector values."

	self updateTitle.
	self updateHeaderBar.
	self updateEvaluator.
	self updatePages
]

{ #category : #accessing }
StObjectInspector >> refreshContent [
	"I force a refresh of current page contents. "

	views selectedPage resetContent
]

{ #category : #accessing }
StObjectInspector >> selectedObject [
	^ self inspectedObject
]

{ #category : #accessing }
StObjectInspector >> selectedPage [

	^ views selectedPage
]

{ #category : #accessing }
StObjectInspector >> selectedPageName [

	^ self selectedPage title
]

{ #category : #private }
StObjectInspector >> setModel: aModel [

	self model: aModel
]

{ #category : #'model access' }
StObjectInspector >> setModelBeforeInitialization: anInspectorModel [

	model := anInspectorModel
]

{ #category : #stepping }
StObjectInspector >> step [

	alivePresentersByPage 
		do: [ :each | each step ]
]

{ #category : #accessing }
StObjectInspector >> title [

	^ self model windowTitle
]

{ #category : #accessing }
StObjectInspector >> trySelectAnyPageNamed: aCollection [
	"Tries to select a the first page of aCollection that exists in the pages list"

	(aCollection asOrderedCollection reject: #isNil) removeDuplicates 
		do: [ :eachName |
			views pages 
				detect: [ :each | each title = eachName ]
				ifFound: [ :aPage | 
					views selectPage: aPage.
					^ self ] ]
]

{ #category : #accessing }
StObjectInspector >> trySelectPageNamed: aString [
	"Tries to select a page with the name indicated. If there is no match, 
	 just stay as before (used to keep the flow when included in a miller list)."

	views pages 
		detect: [ :each | each title = aString ]
		ifFound: [ :aPage | views selectPage: aPage ]
]

{ #category : #'private updating' }
StObjectInspector >> updateEvaluator [

	evaluator beForObject: self model inspectedObject
]

{ #category : #'private updating' }
StObjectInspector >> updateHeaderBar [
		
	header label: self model labelString.
	header toolbarActions: self rootCommandsGroup 
]

{ #category : #'private updating' }
StObjectInspector >> updatePages [

	views 
		pages: self allPages;
		selectPageIndex: 1
]

{ #category : #'private updating' }
StObjectInspector >> updateTitle [
		
	self isRoot ifFalse: [ ^ self ].
	self withWindowDo: [ :window | 
		window title: self title ].
]

{ #category : #'api-events' }
StObjectInspector >> whenActivatedDo: aBlock [

	activationBlock := aBlock
]
