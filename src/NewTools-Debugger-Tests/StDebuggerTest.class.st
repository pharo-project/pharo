Class {
	#name : #StDebuggerTest,
	#superclass : #TestCase,
	#instVars : [
		'session',
		'debugger',
		'oldFastTDD'
	],
	#category : #'NewTools-Debugger-Tests-Presenters'
}

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
StDebuggerTest >> executionControlCommandFrom: aDebugger [

	^ debugger toolbarCommandGroup allCommands select: [ :c | 
		  #( 'Proceed' 'Into' 'Over' 'Through' ) includes: c name ]
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
	super setUp.	
	session := StTestDebuggerProvider new sessionForDebuggerTests.
	oldFastTDD := self debuggerClass fastTDD.
	self debuggerClass fastTDD: true
]

{ #category : #running }
StDebuggerTest >> tearDown [

	session clear.
	self debuggerClass fastTDD: oldFastTDD.
	debugger ifNotNil: [ 
		debugger close.
		debugger session ifNotNil: [ :s | s clear ] ].
	StTestDebuggerProvider removeSelector: #foobar.
	super tearDown
]

{ #category : #'tests - actions' }
StDebuggerTest >> testClose [
	(self debuggerOn: session) close.
	self deny: (SystemAnnouncer uniqueInstance hasSubscriber: debugger)
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
	self assert: contextItems size equals: 7. 
	
	"First nodes are temps"
	self assert: (contextItems at: 1) class equals: StDebuggerInspectorTempNode.
	self assert: (contextItems at: 2) class equals: StDebuggerInspectorTempNode.
	self assert: (contextItems at: 3) class equals: StDebuggerInspectorTempNode.
	self assert: (contextItems at: 4) class equals: StDebuggerInspectorTempNode.
	
	"receiver inst var"		
	self assert: (contextItems at: 5) rawValue identicalTo: session shortStack first receiver session.
	
	"stackTop"
	self assert: (contextItems at: 6) rawValue equals: 1.
	
	"thisContext"
	self assert: contextItems last rawValue identicalTo: dbg currentContext
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

{ #category : #'tests - initialization' }
StDebuggerTest >> testFastTDD [
	self debuggerClass fastTDD: nil.
	self deny: self debuggerClass fastTDD
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

{ #category : #'tests - stack table' }
StDebuggerTest >> testPrintReceiverClassInContext [
	|ctx result|
	ctx := Context sender: nil receiver: Set new method: (Collection>>#add:) arguments: Array new. 
	result := self debugger printReceiverClassInContext: ctx.
	self assert: result equals: 'Set (Collection)'.
	
	ctx := Context sender: nil receiver: Set new method: (Set>>#add:) arguments: Array new. 
	result := self debugger printReceiverClassInContext: ctx.
	self assert: result equals: 'Set'.
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

{ #category : #'tests - actions' }
StDebuggerTest >> testRequestClassFrom [

	self
		assert: (self debugger requestClassFrom: self class)
		identicalTo: self class
]

{ #category : #'tests - actions' }
StDebuggerTest >> testRequestProtocolIn [

	self
		assert: (self debugger requestProtocolIn: self class)
		equals: Protocol unclassified
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
	| columns classColumn methodColumn method block context |
	self debugger session: session.
	columns := self initializedDebugger stackTable columns.
	classColumn := columns first.
	methodColumn := columns second.
	"The following column was removed to experiment a new layout, just keeping the test in case of roolback."
	"senderColumn := columns third."
	method := self class >> #testStackTableElementsPrinting.
	block := [ :a | a ].
	
	context := (Context newForMethod: method) setSender: self receiver: self method: method arguments: #(  ).
	self assert: (classColumn evaluation value: context) equals: self class name.
	self assert: (methodColumn evaluation value: context) equals: method selector.
	"The following column was removed to experiment a new layout, just keeping the test in case of roolback."
	"self assert: (senderColumn evaluation value: context) equals: self stDisplayString."
	
	context := (Context newForMethod: method) setSender: self receiver: self method: block method arguments: #(  ).
	self assert: (classColumn evaluation value: context) equals: self class name.
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
	self skip

]

{ #category : #'tests - toolbar' }
StDebuggerTest >> testToolbarDisplayMode [
	debugger := self debuggerOn: session.
	self
		assert: debugger toolbar displayMode
		equals: debugger application toolbarDisplayMode
]

{ #category : #'tests - toolbar' }
StDebuggerTest >> testToolbarInDNUContext [
	|commands|
	session clear.
	session := StTestDebuggerProvider new sessionWithDNUAfterStep.

	self debuggerOn: session.
	debugger buildContextMenus.		
	commands := self executionControlCommandFrom: debugger.	

	"We're not in a DNU, buttons are enabled"
	commands do: [ :c| self assert: c presenter isEnabled].

	"We step into the DNU, buttons should be disabled"
	debugger stepInto.
	debugger stepOver.
	commands do: [ :c| self deny: c presenter isEnabled].

	"We define the missing method from outside the debugger, buttons should automatically be enabled"
	StTestDebuggerProvider compile: 'foobar ^self'.		
	commands do: [ :c| self assert: c presenter isEnabled].
	
	
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
