"
I implement a pre debug window for GTDebugger. I reuse the debug window for the SpecDebugger and adapt the way stack frames are obtained from the debugger.

I receive a instance to a full debugger that I'll open if the user continues debugging.

"
Class {
	#name : #GTSpecPreDebugWindow,
	#superclass : #DynamicComposablePresenter,
	#instVars : [
		'debugger',
		'title',
		'message'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #specs }
GTSpecPreDebugWindow class >> spec [
	<spec: #default>
		^ SpecLayout composed
			yourself
]

{ #category : #'actions lookup' }
GTSpecPreDebugWindow >> actionsForPragmas: aSymbolsCollection [

	^ (self session class debuggingActionsForPragmas: aSymbolsCollection for: self debugger)
				
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> buildButtonWidgetsSpecForActions: aCollection [

	^ aCollection
		inject: OrderedCollection new
		into: [ :specs :action |
			specs 
				add: action specId;
			 	add: action specModel;
				yourself ]
	
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> buildNotifierPaneWidgetsSpec [
	
	^ self message 
		ifNil: [ {#stackPane. #MultiColumnListPresenter} ] 
		ifNotNil: [ {#codePane. #TextPresenter} ]
]

{ #category : #actions }
GTSpecPreDebugWindow >> clear [

	self debugger ifNotNil: [ :aDebugger | aDebugger windowIsClosing ]
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> clearWidget [

	self widgets removeAll.
	
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: self emptyLayout	

]

{ #category : #actions }
GTSpecPreDebugWindow >> close [
	
	self window delete
]

{ #category : #printing }
GTSpecPreDebugWindow >> columnsFor: aContext [

	^ { 
		self debugger formatStackClassColumnForContext: aContext.
		self debugger formatStackMethodColumnForContext: aContext.
		self debugger formatStackExtraColumnForContext: aContext  }
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> createButtonWidgetsForActions: aCollection [

	self instantiatePresenters: (self buildButtonWidgetsSpecForActions: aCollection).
	aCollection do: [ :action | 
		self initializeButtonWidget: (self widgets at: action specId) forAction: action ]
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> createNotifierPaneWidgets [

	self instantiatePresenters: self buildNotifierPaneWidgetsSpec.
	(self widgets keys includes: #codePane) ifTrue: [ 
		(self widgets at: #codePane) beForText.
	].
	self widgets keysDo: [ :key | 
		(self perform: ('initialize', key asString capitalized) asSymbol) ]
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> createWidgetsForActions: aCollection [

	self createNotifierPaneWidgets.
	self createButtonWidgetsForActions: aCollection.

	self setFocusOrderForActions: aCollection
]

{ #category : #accessing }
GTSpecPreDebugWindow >> debugger [

	^ debugger value
]

{ #category : #accessing }
GTSpecPreDebugWindow >> debugger: anOnject [

	debugger value: anOnject
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> dynamicLayoutForActions: aCollection [
	"Create a two column layout for the subwidgets."

	^ SpecLayout composed
		newColumn: [ :c | 
			c newRow: [ :row |
				aCollection do: [ :anAction | row add: anAction specId ] ] height: self class toolbarHeight.
	  		c newRow: [ :row | row add: self notifierPaneWidgetId] ]
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> emptyLayout [
	
	^ SpecLayout composed

]

{ #category : #'initialization widgets' }
GTSpecPreDebugWindow >> filteredStack [

	^ (self debugger session stackOfSize: 50 ) select: [ :aContext | (aContext method hasPragmaNamed: #debuggerCompleteToSender) not ]
]

{ #category : #'initialization widgets' }
GTSpecPreDebugWindow >> firstContextToOpen [

	self filteredStack ifEmpty: [ ^ nil ].
	^ self filteredStack first
]

{ #category : #api }
GTSpecPreDebugWindow >> initialExtent [

	^ 700 @ 180
]

{ #category : #initialization }
GTSpecPreDebugWindow >> initialize [

	debugger := nil asValueHolder.
	super initialize.
	
	self whenWindowChanged: [:w | w whenClosedDo: [ self clear ] ].
	self bindKeyCombination: Character escape asKeyCombination toAction: [ self delete ]
]

{ #category : #'initialization widgets' }
GTSpecPreDebugWindow >> initializeButtonWidget: aButtonPresenter forAction: aDebuggingAction [

	aButtonPresenter
		debugAction: aDebuggingAction
]

{ #category : #'initialization widgets' }
GTSpecPreDebugWindow >> initializeCodePane [
		
	self codePane
		text: self message
		
]

{ #category : #initialization }
GTSpecPreDebugWindow >> initializePresenter [

	super initializePresenter.
	
	debugger whenChangedDo: [ :aDebugger |
		aDebugger 
			ifNil: [ self clearWidget ] 
			ifNotNil:  [ self rebuildWidget ]	 ]

]

{ #category : #'initialization widgets' }
GTSpecPreDebugWindow >> initializeStackPane [
	
	self stackPane
		displayBlock: [ :aContext | self columnsFor: aContext ];
		items: self filteredStack ;
		whenSelectedItemChanged: [ :aContext | 
			"Set the selection before, as debugAction removes the link with the debugger. "
			self debugger stackPresentation selection: aContext.
			self openFullDebugger ]
		
]

{ #category : #accessing }
GTSpecPreDebugWindow >> message [

	^ message
]

{ #category : #accessing }
GTSpecPreDebugWindow >> message: aString [
	"Set a message in the pre debug window. 
	If this is nil the notifier pane will display a text editor showing the message (Warning signal: 'Display this text'),
	otherwise the stack will be displayed."
	
	message := aString
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> notifierPaneWidgetId [
	"Return the id of the notifier widget. Assume there is just one widget that is not a button,
	which represents the notifier."
	
	self buildNotifierPaneWidgetsSpec pairsDo: [ :key :value |
		(self widgets keys includes: key) ifTrue: [ ^ key ] ].
	^ nil
]

{ #category : #actions }
GTSpecPreDebugWindow >> openFullDebugger [
	| currentDebugger |
	currentDebugger := self debugger.
	self setDebuggerToFirstNonFilteredContext.
	self debugger: nil.
	self close.
	currentDebugger open
]

{ #category : #'actions lookup' }
GTSpecPreDebugWindow >> preDebuggingActions [
	"Return a collection of debug actions constructed based on the pragmas 'self preDebuggingActionsPragmas'. 
	These actions should understand the message #preDebugWindow:"
	
	^ (self actionsForPragmas: self preDebuggingActionsPragmas) 
		do: [ :each | 
			each preDebugWindow: self ]
]

{ #category : #'actions lookup' }
GTSpecPreDebugWindow >> preDebuggingActionsPragmas [

	^ { #preDebuggingAction }
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> rebuildWidget [
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
GTSpecPreDebugWindow >> session [

	^ self debugger session
]

{ #category : #actions }
GTSpecPreDebugWindow >> setDebuggerToFirstNonFilteredContext [
	"I set the debugger to the first non filtered stack"
	| currentDebugger |

	currentDebugger := self debugger.

	(currentDebugger stackPresentation selection isNil
		or: [ currentDebugger stackPresentation selection method
				hasPragmaNamed: #debuggerCompleteToSender ])
		ifTrue: [ currentDebugger stackPresentation selection: self firstContextToOpen ]
]

{ #category : #'building widgets' }
GTSpecPreDebugWindow >> setFocusOrderForActions: aCollection [

	self focusOrder removeAll.
	aCollection do: [ :action | 
		self focusOrder add: (self perform: action specId) ].	
	self focusOrder add: (self perform: self notifierPaneWidgetId)
]

{ #category : #api }
GTSpecPreDebugWindow >> setTitle: aString [

	title := aString.
	self updateTitle 
]

{ #category : #accessing }
GTSpecPreDebugWindow >> stackPane [
	^widgets at: #stackPane
]

{ #category : #api }
GTSpecPreDebugWindow >> title [

	^ title 
]
