Class {
	#name : #StDebuggerTest,
	#superclass : #TestCase,
	#instVars : [
		'session',
		'debugger'
	],
	#category : #'NewTools-Debugger-Tests-Presenters'
}

{ #category : #'helpers - debugger control' }
StDebuggerTest >> clickStepInto [
	self debugger updateToolbar.
	(self debugger toolbar items detect:[:item| item label = 'Into']) click
]

{ #category : #'helpers - debugger control' }
StDebuggerTest >> clickStepInto: n [
	n timesRepeat: [ self clickStepInto ]
]

{ #category : #'helpers - debugger control' }
StDebuggerTest >> clickStepOver [
	self debugger updateToolbar.
	(self debugger toolbar items detect:[:item| item label = 'Over']) click
]

{ #category : #'helpers - debugger control' }
StDebuggerTest >> clickStepOver: n [
	n timesRepeat: [ self clickStepOver ]
]

{ #category : #accessing }
StDebuggerTest >> debugger [
	^ debugger ifNil: [ debugger := self debuggerClass basicNew ]
]

{ #category : #accessing }
StDebuggerTest >> debuggerClass [
	^ StDebugger
]

{ #category : #accessing }
StDebuggerTest >> debuggerOn: aDebugSession [
	^ self debugger
		  session: aDebugSession;
		  application: self debuggerClass currentApplication;
		  initialize;
		  buildContextMenus;
		  yourself
]

{ #category : #accessing }
StDebuggerTest >> initializedDebugger [
	| dbg |
	dbg := self debugger.
	dbg application: self debuggerClass currentApplication.
	dbg initialize.
	^ dbg
]

{ #category : #accessing }
StDebuggerTest >> inspectorTableFor: inspector [
	|contextInspector|
	contextInspector := (inspector class slotNamed: #contextInspector) read: inspector.
	^(contextInspector class slotNamed: #attributeTable) read: contextInspector
]

{ #category : #running }
StDebuggerTest >> setUp [
	| context process |
	super setUp.
	context := [ | i |
	i := 1 squared.
	i = 1
		ifTrue: [ | j |
			j := i + 1 ].
	^ 1 + i] asContext.
	process := Process
		forContext: context
		priority: Processor userInterruptPriority.
	session := DebugSession
		named: 'test session'
		on: process
		startedAt: context
]

{ #category : #running }
StDebuggerTest >> tearDown [

	session clear.
	super tearDown
]

{ #category : #'tests - code pane' }
StDebuggerTest >> testCodeChangeAfterContextChange [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - code pane' }
StDebuggerTest >> testCodeModification [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - code pane' }
StDebuggerTest >> testCodeSelectionAfterStepOver [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - context inspector' }
StDebuggerTest >> testContextChangedAfterStepIn [ 
	| dbg |
	dbg := self debuggerOn: session.
	dbg buildContextMenus.	
	
	dbg stepInto. "We enter the block context"
	self assert: dbg inspector inspectedObject context identicalTo: session shortStack first.
	
	dbg stepInto. "We step into the first message"
	self assert: dbg inspector inspectedObject context identicalTo:session shortStack first.

]

{ #category : #'tests - context inspector' }
StDebuggerTest >> testContextChangedAfterStepOverAndReturn [
	| dbg currentContext |
	dbg := self debuggerOn: session.	
		
	dbg stepInto; stepInto. "getting into Number>>squared"
	currentContext := dbg inspector inspectedObject context.
	dbg stepOver; stepOver.  "stepping over ^self * self (i.e. returning)"
	self
		deny: dbg inspector inspectedObject context
		equals: currentContext
]

{ #category : #'tests - context inspector' }
StDebuggerTest >> testContextTempVarList [
	| dbg contextItems inspectorTable|
	dbg := self debuggerOn: session.
	inspectorTable := dbg inspector getRawInspectorPresenterOrNil attributeTable.
	
	5 timesRepeat: [ dbg stepInto  ].	
	contextItems := inspectorTable roots.
	self assert: contextItems first rawValue equals: session shortStack first receiver. "receiver"	
	self assert: (contextItems second rawValue at: #i) equals: 1.
	self assert: contextItems third rawValue equals: 1. "stackTop"
	self assert: contextItems last rawValue equals: dbg currentContext.
]

{ #category : #'tests - context inspector' }
StDebuggerTest >> testContextUnchangedAfterStepOver [
	| dbg currentContext |
	dbg := self debuggerOn: session.	
	
	dbg stepInto; stepInto. "getting into Number>>squared"
	currentContext := dbg inspector inspectedObject.
	"stepping over one time ^self * self (i.e. we are on the return node but still in the same context)"
	dbg stepOver. 
	self assert: dbg inspector inspectedObject ==  currentContext
]

{ #category : #'tests - actions' }
StDebuggerTest >> testCopyStackToClipboard [
	debugger := self debuggerOn: session.
	debugger copyStackToClipboard.
	self
		assert: Clipboard clipboardText string
		equals: (String streamContents: [ :s | 
				 session interruptedContext shortDebugStackOn: s ])
]

{ #category : #'tests - initialization' }
StDebuggerTest >> testDebuggerApplication [
	self
		assert: (self debuggerOn: session) application
		identicalTo: StDebugger currentApplication
]

{ #category : #'tests - code pane' }
StDebuggerTest >> testDiscardCodeModification [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - extensions' }
StDebuggerTest >> testExtendedToolsClassesFor [
	self
		assertCollection:
			(StDebugger
				collectExtendedToolsPragmaFor: #debuggerExtensionTestPragma)
		includesAll: (StDummyDebuggerPresenter class>>#debuggerExtensionTestPragma) pragmas 
]

{ #category : #'tests - extensions' }
StDebuggerTest >> testExtendedToolsPragma [
	self assert: StDebugger extendedToolsPragma equals: #debuggerExtensionOrder:showByDefault:
]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testInspectorHeaderLabel [
	| dbg displayedLabel |
	dbg := self debuggerOn: session.
	displayedLabel := ((((dbg inspector instVarNamed: #inspector) 
		                     instVarNamed: #millerList) presenters first 
		                    instVarNamed: #header) instVarNamed: #titleLabel)
		                  label.
	self assert: (dbg inspector model labelString copyFrom: 1 to: 12) equals: 'Receiver in:'.	
	self assert: (displayedLabel copyFrom: 1 to: 12) equals: 'Receiver in:'.
]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testInspectorSelfBinding [
	| dbg evaluator receiver |
	dbg := self debuggerOn: session.
	evaluator := (dbg inspector instVarNamed: #inspector) millerList
		             presenters first evaluator.
	receiver := dbg inspector model inspectedObject context receiver.
	self assert: evaluator doItReceiver identicalTo: receiver.
	self assert: evaluator behavior identicalTo: receiver class
]

{ #category : #'tests - extensions' }
StDebuggerTest >> testInstantiateExtensionToolsPage [
	|dbg toolPage|
	dbg := self debugger.
	toolPage := dbg instantiateExtensionToolsPage: StDummyDebuggerPresenter.
	self assert: toolPage class identicalTo: SpNotebookPage.
	self assert: toolPage presenterProvider value class identicalTo: StDummyDebuggerPresenter.
	self assert: toolPage title equals: StDummyDebuggerPresenter debuggingExtensionToolName.
	self assertCollection: dbg extensionTools includesAll: { toolPage presenterProvider value}.
	 
]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testReceiverChangedAfterStepIn [ 
	| dbg |
	dbg := self debuggerOn: session.
	
	dbg stepInto. "We enter the block context"
	self assert: dbg inspector inspectedObject context receiver identicalTo: session shortStack first receiver.
	
	dbg stepInto. "We step into the first message"
	self assert: dbg inspector inspectedObject context receiver identicalTo: session shortStack first receiver.


]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testReceiverChangedAfterStepOverAndReturn [
	| dbg currentReceiver |
	dbg := self debuggerOn: session.	
	
	dbg stepInto; stepInto. "getting into Number>>squared"
	currentReceiver := dbg inspector inspectedObject.
	dbg stepOver; stepOver. "stepping over ^self * self (i.e. returning)"
	self
		deny: dbg inspector inspectedObject
		equals: currentReceiver
]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testReceiverInitialRawInspectionSelection [
]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testReceiverInspectorMenu [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testReceiverList [
	| dbg currentReceiver |
	dbg := self debuggerOn: session.	
	
	dbg stepInto; stepInto. "getting into Number>>squared"
	currentReceiver := dbg inspector inspectedObject context receiver.	
	self assert: currentReceiver equals: 1

]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testReceiverUnchangedAfterStepOver [ 
	| dbg currentReceiver |
	dbg := self debuggerOn: session.	
	
	dbg stepInto; stepInto. "getting into Number>>squared"
	currentReceiver := dbg inspector inspectedObject context receiver.
	"stepping over one time ^self * self (i.e. we are on the return node but still in the same context)"
	dbg stepOver. 
	self assert: dbg inspector inspectedObject context receiver identicalTo:  currentReceiver

]

{ #category : #'tests - extensions' }
StDebuggerTest >> testRegisterExtensionTool [
	|dbg object|
	dbg := self debugger.
	self assertEmpty: dbg extensionTools.
	dbg registerExtensionTool: (object := Object new).
	self assert: dbg extensionTools size equals: 1.
	self assertCollection: dbg extensionTools includesAll: { object }
]

{ #category : #'tests - raw inspection' }
StDebuggerTest >> testRestoreReceiverRawInspectionSelection [
]

{ #category : #'tests - raw inspection' }
StDebuggerTest >> testRestoreReceiverRawInspectionSelectionAfterContextChange [
]

{ #category : #'tests - session' }
StDebuggerTest >> testReturnEnteredValue [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - code pane' }
StDebuggerTest >> testSaveCodeModification [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - raw inspection' }
StDebuggerTest >> testSaveReceiverRawInspectionSelection [
]

{ #category : #'tests - raw inspection' }
StDebuggerTest >> testSaveReceiverRawInspectionSelectionAfterContextChange [
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testSelectLastStackElementWithMoreElements [
	| dbg stackTable selectedItem |
	dbg := self debuggerOn: session.
	stackTable := dbg stackTable.
	
	dbg stepInto; stepInto.
	stackTable items: (session stackOfSize: 1).
	
	self assert: stackTable items size equals: 1.
	self assert: stackTable items last equals: session stack first.	
		
	"As we select the last element of the stack table, 
	it should expand the size of the displayed stack"
	selectedItem := stackTable items last.
	stackTable selectItem: selectedItem.
	self assert: stackTable items size equals: session stack size.
	self assert: stackTable items equals: session stack.
	self assert: stackTable selection selectedItem identicalTo: selectedItem

	
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testSelectLastStackElementWithoutMoreElements [
	| dbg stackTable startingStackSize |
	dbg := self debuggerOn: session.
	stackTable := dbg stackTable.
	startingStackSize := stackTable items size.
	stackTable selectItem: stackTable items last.
	
	self assert: stackTable items size equals: startingStackSize
]

{ #category : #'tests - code pane' }
StDebuggerTest >> testSelectionMenu [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testStackTableAfterReturn [
	| dbg stackTableItems stackSizeAfterStepIn |
	dbg := self debuggerOn: session.

	"we dive into a method"
	dbg stepInto; stepInto.
	stackSizeAfterStepIn := dbg stackTable items size.

	"we return from it"
	dbg stepOver; stepOver.
	stackTableItems := dbg stackTable items.
	self assert: dbg stackTable selection selectedIndex equals: 1.
	self assert: dbg stackTable selection selectedItem identicalTo: session shortStack first.
	self assert: stackTableItems size equals: stackSizeAfterStepIn - 1.
	self assert: stackTableItems equals: session shortStack
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testStackTableAfterStepIn [
	| dbg stackTableItems startingStackSize |
	dbg := self debuggerOn: session.
	startingStackSize := dbg stackTable items size.
	dbg stepInto; stepInto.
	stackTableItems := dbg stackTable items.
	self assert: dbg stackTable selection selectedIndex equals: 1.
	self
		assert: dbg stackTable selection selectedItem == session shortStack first.
	self assert: stackTableItems size equals: startingStackSize + 1.
	self assert: stackTableItems equals: session shortStack
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testStackTableElementsPrinting [
	| columns classColumn methodColumn senderColumn method block context |
	self debugger session: session.
	columns := self initializedDebugger stackTable columns.
	classColumn := columns first.
	methodColumn := columns second.
	"The following column was removed to experiment a new layout, just keeping the test in case of roolback."
	"senderColumn := columns third."
	method := self class >> #testStackTableElementsPrinting.
	block := [ :a | a ].
	
	context := (Context newForMethod: method) setSender: self receiver: self method: method arguments: #(  ).
	self assert: (classColumn evaluation value: context) equals: self class.
	self assert: (methodColumn evaluation value: context) equals: method selector.
	"The following column was removed to experiment a new layout, just keeping the test in case of roolback."
	"self assert: (senderColumn evaluation value: context) equals: self stDisplayString."
	
	context := (Context newForMethod: method) setSender: self receiver: self method: block method arguments: #(  ).
	self assert: (classColumn evaluation value: context) equals: self class.
	self assert: (methodColumn evaluation value: context) equals: block sourceNode sourceCode.
	"The following column was removed to experiment a new layout, just keeping the test in case of roolback."
	"self assert: (senderColumn evaluation value: context) equals: self stDisplayString."
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testStackTableInitialization [
	self assert: self debugger stackTable isNil
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testStackTableSelectionChanged [
	| dbg |
	dbg := self debuggerOn: session.
	dbg stepInto; stepInto.
	dbg stackTable selectItem: dbg stackTable items second.
	self
		assert:
			dbg inspector inspectedObject context
				identicalTo: dbg stackTable selection selectedItem.
	self
		assert:
			dbg inspector inspectedObject context receiver
				identicalTo: dbg stackTable selection selectedItem receiver.
	self
		assert: dbg code text
		equals: dbg stackTable items second sourceCode
]

{ #category : #'tests - stack table' }
StDebuggerTest >> testStackTableWithNewSession [
	self debugger session: session.
	self
		assert: (self initializedDebugger stackTable) items
		equals: session shortStack.
	self
		assert: self debugger stackTable selection selectedIndex
		equals: 1.
	self
		assert:
			self debugger stackTable selection selectedItem
				identicalTo: session shortStack first
]

{ #category : #'tests - toolbar' }
StDebuggerTest >> testToolbarDependingOnContext [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - toolbar' }
StDebuggerTest >> testToolbarDisplayMode [
	debugger := self debuggerOn: session.
	self
		assert: debugger toolbar displayMode
		equals: debugger application toolbarDisplayMode
]

{ #category : #'tests - code pane' }
StDebuggerTest >> testUnauthorizedCodeModification [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - receiver inspector' }
StDebuggerTest >> testUpdateLayoutForContextsIfAssertionFailure [
	| assertionFailure currentLayout expectedLayout |
	debugger := StTestDebuggerProvider new
		            debuggerWithFailingAssertionContext.
	session := debugger session.
	debugger application: StDebugger currentApplication.
	debugger initialize.
	debugger updateInspectorFromContext: session interruptedContext.
	assertionFailure := debugger inspector assertionFailure.
	currentLayout := debugger inspector layout.
	expectedLayout := StDebuggerInspector assertionFailureSpec.
	self assert: currentLayout direction equals: expectedLayout direction.
	self
		assertCollection: currentLayout children
		equals: expectedLayout children.
	self assert: currentLayout position equals: expectedLayout position.
	self
		assert: currentLayout children size
		equals: expectedLayout children size.
	self
		assert: assertionFailure actualObject
		equals:
		((session interruptedContext tempNamed: #aStringOrBlock) at: 2).
	self
		assert: assertionFailure expectedObject
		equals:
		((session interruptedContext tempNamed: #aStringOrBlock) at: 1)
]
