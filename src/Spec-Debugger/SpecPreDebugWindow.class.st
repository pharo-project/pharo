"
I am the window that shows up when the debugger is launched.  
I show you the first lines of the stack trace that caused my apparition (or a text editor with a message), and a set of buttons created by loading all debugging actions annotated with the pragmas 'self preDebuggingActionsPragmas' (e.g. resume the execution, abandon the execution).

To see me, evaluate:
2/0.

Some details about my implementation.

- I hold a reference to the debugger than created my;
- changing the debuger retriggers the construction of this window;
- before doing this users should set the attribute 'message' as my structure depens on it;
- changing the debugger should be done only after this window was open. E.g.:

SpecPreDebugWindow new 
		setTitle: aTitle;
		message: aMessage;
		openWithSpec;
		debugger: aDebugger.
		
- an example of manually opening this window:

[ | context process  debugger |

context := [ 20 factorial ] asContext.

process := Process 
	forContext: context
	priority: Processor userInterruptPriority.

debugger := SpecDebugger new
	process: process 
	controller:  nil
	context: context.
	
SpecPreDebugWindow new 
		setTitle: 'A simulated error';
		message: nil;
		openWithSpec;
		debugger: debugger
 ] fork.
"
Class {
	#name : #SpecPreDebugWindow,
	#superclass : #DynamicComposablePresenter,
	#instVars : [
		'debugger',
		'title',
		'message'
	],
	#category : #'Spec-Debugger'
}

{ #category : #accessing }
SpecPreDebugWindow class >> debuggerClass [

	^ SpecDebugger
]

{ #category : #specs }
SpecPreDebugWindow class >> spec [
	<spec: #default>
		^ SpecLayout composed
			yourself
]

{ #category : #icons }
SpecPreDebugWindow class >> taskbarIconName [

	^ self debuggerClass taskbarIconName
]

{ #category : #'actions lookup' }
SpecPreDebugWindow >> actionsForPragmas: aSymbolsCollection [

	^ (self session class debuggingActionsForPragmas: aSymbolsCollection for: self debugger)
				
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> buildButtonWidgetsSpecForActions: aCollection [

	^ aCollection
		inject: OrderedCollection new
		into: [ :specs :action |
			specs 
				add: action specId;
			 	add: action specModel;
				yourself ]
	
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> buildNotifierPaneWidgetsSpec [
	
	^ self message 
		ifNil: [ {#stackPane. #MultiColumnListPresenter} ] 
		ifNotNil: [ {#codePane. #TextPresenter} ]
]

{ #category : #actions }
SpecPreDebugWindow >> clear [

	self debugger ifNotNil: [ :aDebugger | aDebugger clear ]
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> clearWidget [

	self widgets removeAll.
	
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: self emptyLayout	

]

{ #category : #actions }
SpecPreDebugWindow >> close [
	
	self window delete
]

{ #category : #printing }
SpecPreDebugWindow >> columnsFor: anEntry [
	
	^ self debugger stack columnsFor: anEntry
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> createButtonWidgetsForActions: aCollection [

	self instantiatePresenters: (self buildButtonWidgetsSpecForActions: aCollection).
	aCollection do: [ :action | 
		self initializeButtonWidget: (self widgets at: action specId) forAction: action ]
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> createNotifierPaneWidgets [

	self instantiatePresenters: self buildNotifierPaneWidgetsSpec.
	(self widgets keys includes: #codePane) ifTrue: [ 
		(self widgets at: #codePane) beForText.
	].
	self widgets keysDo: [ :key | 
		(self perform: ('initialize', key asString capitalized) asSymbol) ]
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> createWidgetsForActions: aCollection [

	self createNotifierPaneWidgets.
	self createButtonWidgetsForActions: aCollection.

	self setFocusOrderForActions: aCollection
]

{ #category : #accessing }
SpecPreDebugWindow >> debugger [

	^ debugger value
]

{ #category : #accessing }
SpecPreDebugWindow >> debugger: anOnject [

	debugger value: anOnject
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> dynamicLayoutForActions: aCollection [
	"Create a two column layout for the subwidgets."

	^ SpecLayout composed
		newColumn: [ :c | 
			c newRow: [ :row |
				aCollection do: [ :anAction | row add: anAction specId ] ] height: self class toolbarHeight.
	  		c newRow: [ :row | row add: self notifierPaneWidgetId] ]
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> emptyLayout [
	
	^ SpecLayout composed

]

{ #category : #api }
SpecPreDebugWindow >> initialExtent [

	^ 700 @ 156
]

{ #category : #initialization }
SpecPreDebugWindow >> initialize [

	debugger := nil asValueHolder.
	super initialize.
	
	self whenWindowChanged: [:w | w whenClosedDo: [ self clear ] ].
	self bindKeyCombination: Character escape asKeyCombination toAction: [ self delete ]
]

{ #category : #'initialization widgets' }
SpecPreDebugWindow >> initializeButtonWidget: aButtonPresenter forAction: aDebuggingAction [

	aButtonPresenter
		debugAction: aDebuggingAction
]

{ #category : #'initialization widgets' }
SpecPreDebugWindow >> initializeCodePane [
		
	self codePane
		text: self message
		
]

{ #category : #initialization }
SpecPreDebugWindow >> initializePresenter [

	super initializePresenter.
	
	debugger whenChangedDo: [ :aDebugger |
		aDebugger 
			ifNil: [ self clearWidget ] 
			ifNotNil:  [ self rebuildWidget ]	 ]

]

{ #category : #'initialization widgets' }
SpecPreDebugWindow >> initializeStackPane [
	
	self stackPane
		displayBlock: [ :aContext | self columnsFor: aContext ];
		items: (self debugger stack filteredStackOfSize: self debugger stack class defaultStackSize) ;
		whenSelectedItemChanged: [ :aContext | 
			"Set the selection before, as debugAction removes the link with the debugger. "
			self debugger stack setSelectedItem: aContext.
			self openFullDebugger ]
		
]

{ #category : #accessing }
SpecPreDebugWindow >> message [

	^ message
]

{ #category : #accessing }
SpecPreDebugWindow >> message: aString [
	"Set a message in the pre debug window. 
	If this is nil the notifier pane will display a text editor showing the message (Warning signal: 'Display this text'),
	otherwise the stack will be displayed."
	
	message := aString
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> notifierPaneWidgetId [
	"Return the id of the notifier widget. Assume there is just one widget that is not a button,
	which represents the notifier."
	
	self buildNotifierPaneWidgetsSpec pairsDo: [ :key :value |
		(self widgets keys includes: key) ifTrue: [ ^ key ] ].
	^ nil
]

{ #category : #actions }
SpecPreDebugWindow >> openFullDebugger [
	| currentDebugger |
	
	currentDebugger := self debugger.
	self debugger: nil.
	self close.
	currentDebugger 
		openWithSpec;
		updateToolbar;
		updateCodeFromContext;
		label: title
]

{ #category : #'actions lookup' }
SpecPreDebugWindow >> preDebuggingActions [
	"Return a collection of debug actions constructed based on the pragmas 'self preDebuggingActionsPragmas'. 
	These actions should understand the message #preDebugWindow:"
	
	^ (self actionsForPragmas: self preDebuggingActionsPragmas) 
		do: [ :each | 
			each preDebugWindow: self ]
]

{ #category : #'actions lookup' }
SpecPreDebugWindow >> preDebuggingActionsPragmas [

	^ { #preDebuggingAction }
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> rebuildWidget [
	| debuggingActions |
	
	debuggingActions := self preDebuggingActions.
	self widgets removeAll.
	self createWidgetsForActions: debuggingActions.
	 
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: (self dynamicLayoutForActions: debuggingActions).
	
	self takeKeyboardFocus. 

]

{ #category : #accessing }
SpecPreDebugWindow >> session [

	^ self debugger session
]

{ #category : #'building widgets' }
SpecPreDebugWindow >> setFocusOrderForActions: aCollection [

	self focusOrder removeAll.
	aCollection do: [ :action | 
		self focusOrder add: (self perform: action specId) ].	
	self focusOrder add: (self perform: self notifierPaneWidgetId)
]

{ #category : #api }
SpecPreDebugWindow >> setTitle: aString [

	title := aString.
	self updateTitle 
]

{ #category : #accessing }
SpecPreDebugWindow >> stackPane [
	^widgets at: #stackPane
]

{ #category : #api }
SpecPreDebugWindow >> title [

	^ title 
]
