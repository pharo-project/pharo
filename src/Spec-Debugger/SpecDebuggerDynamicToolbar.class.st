"
A toolbar that loads its actions dynamically based on pragmas. Only actions annotated with 'self debuggingActionsPragmas' that answer true to the message #appliesToDebugger: are taken into account. 

The toolbar updates itsetf every time a new value is put in the session (even if it is the same value).  If the session is set before opening the toolbar (before a spec is created) the message #update should be sent to the toolbar.

Each debugging actions is displayed by using a SpecDebugActionButton.


"
Class {
	#name : #SpecDebuggerDynamicToolbar,
	#superclass : #DynamicComposablePresenter,
	#instVars : [
		'sessionHolder'
	],
	#category : #'Spec-Debugger'
}

{ #category : #specs }
SpecDebuggerDynamicToolbar class >> spec [
	<spec: #default>
		^ SpecLayout composed
]

{ #category : #'actions lookup' }
SpecDebuggerDynamicToolbar >> actionsForPragmas: aSymbolsCollection [

	^ (self session class debuggingActionsForPragmas: aSymbolsCollection for: self debugger)
				
]

{ #category : #'building widgets' }
SpecDebuggerDynamicToolbar >> buildButtonWidgetsSpecForActions: aCollection [
	
	^ aCollection
		inject: OrderedCollection new
		into: [ :specs :action |
			specs 
				add: action specId;
			 	add: action specModel;
				yourself ]

	
]

{ #category : #'building widgets' }
SpecDebuggerDynamicToolbar >> clearToolbar [

	self widgets removeAll.
	
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: self emptyLayout	

]

{ #category : #'building widgets' }
SpecDebuggerDynamicToolbar >> createWidgetsForActions: aCollection [
	
	self instantiatePresenters: (self buildButtonWidgetsSpecForActions: aCollection).
	aCollection do: [ :action | 
		self initializeButtonWidget: (self widgets at: action specId) forAction: action ].
	self setFocusOrderForActions: aCollection
]

{ #category : #accessing }
SpecDebuggerDynamicToolbar >> currentContext [
	
	^ self debugger currentContext
]

{ #category : #private }
SpecDebuggerDynamicToolbar >> debugger [
	"Return the instance of the debugger containing this toolbar"
	
	^  owner
]

{ #category : #'actions lookup' }
SpecDebuggerDynamicToolbar >> debuggingActions [

	^ self actionsForPragmas: self debuggingActionsPragmas
]

{ #category : #'actions lookup' }
SpecDebuggerDynamicToolbar >> debuggingActionsPragmas [

	^ { #debuggingAction }
]

{ #category : #'building widgets' }
SpecDebuggerDynamicToolbar >> dynamicLayoutForActions: aCollection [

	^ SpecLayout composed
		newRow: [ :r |
			aCollection do: [ :action | 
				r add: action specId ] ]
]

{ #category : #'building widgets' }
SpecDebuggerDynamicToolbar >> emptyLayout [
	
	^ SpecLayout composed

]

{ #category : #initialization }
SpecDebuggerDynamicToolbar >> initialize [

	sessionHolder := nil asValueHolder.
	super initialize
]

{ #category : #'initialization widgets' }
SpecDebuggerDynamicToolbar >> initializeButtonWidget: aButtonPresenter forAction: aDebuggingAction [

	aButtonPresenter
		debugAction: aDebuggingAction
]

{ #category : #initialization }
SpecDebuggerDynamicToolbar >> initializePresenter [
	super initializePresenter.
	
	sessionHolder whenChangedDo: [
		self spec ifNotNil: [ 
			self update	 ] ]

]

{ #category : #'building widgets' }
SpecDebuggerDynamicToolbar >> rebuildToolbar [
	| debuggingActions |

	debuggingActions := self debuggingActions.
	self widgets removeAll.
	self createWidgetsForActions: debuggingActions.
	
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: (self dynamicLayoutForActions: debuggingActions)
]

{ #category : #accessing }
SpecDebuggerDynamicToolbar >> session [
	
	^ sessionHolder value 
]

{ #category : #accessing }
SpecDebuggerDynamicToolbar >> session: aSession [
	
	sessionHolder value: aSession
]

{ #category : #initialization }
SpecDebuggerDynamicToolbar >> setFocusOrderForActions: aCollection [

	self focusOrder removeAll.
	aCollection do: [ :action | 
		self focusOrder add: (self perform: action specId) ]
]

{ #category : #updating }
SpecDebuggerDynamicToolbar >> update [

	self session 
		ifNil: [ self clearToolbar ] 
		ifNotNil: [ self rebuildToolbar ]
	
]
