Class {
	#name : #EDEmergencyDebuggerTest,
	#superclass : #EDTest,
	#category : #'EmergencyDebugger-Tests'
}

{ #category : #helpers }
EDEmergencyDebuggerTest >> errorText: error [ 	
	| errStream |
	errStream := WriteStream on: Text new.
	errStream cr.
	ed writeSeparatorOn: errStream.
	errStream << error description.
	^errStream contents
]

{ #category : #helpers }
EDEmergencyDebuggerTest >> methodDisplayTextStringExample [
	| method expectedTextStream |
	method := debugAPI methodAt: ed stackSelectionIndex.
	expectedTextStream := WriteStream on: Text new.
	ed writeSectionTitle: 'SOURCE' on: expectedTextStream.
	expectedTextStream << method methodClass name.
	expectedTextStream << '>>'.
	expectedTextStream << method sourceCode.
	expectedTextStream cr.
	^expectedTextStream contents asString
]

{ #category : #helpers }
EDEmergencyDebuggerTest >> resultTextStringExample [
	| expectedTextStream |
	expectedTextStream := WriteStream on: Text new.
	expectedTextStream cr.
	ed writeSectionTitle: 'RES' on: expectedTextStream.
	expectedTextStream << 'result'.
	^ expectedTextStream contents asString
]

{ #category : #'debug API' }
EDEmergencyDebuggerTest >> testBasicDebug [
	ed := EDEmergencyDebugger new.
	ed basicDebug: session.
	self assert: ed debugAPI debugger identicalTo: ed.
	self assert: ed isRunning
	
]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposeDisplayText [
	| displayText |
	displayText := ed titleText , ed composeSessionTitle, ed stackText , ed methodText , ed input
		, ed result.
	self assert: ed composeDisplayText equals: displayText
]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposeErrorTextFrom [
	|error rs|
	error := self zeroDivideErrorInstance.
	ed composeErrorTextFrom: error.
	rs := ed errorText readStream.
	self assert: rs next equals: Character cr.
	self assert: (rs next: ed separator asString size) equals: ed separator asString.
	self assert: rs next equals: Character cr.
	self assert: (rs next: error description size) equals: error description.
	self assert: rs atEnd
]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposeMethodText [
	ed composeMethodText.
	self
		assert: ed methodText asString
		equals: self methodDisplayTextStringExample
]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposePromptWith [
	| rs |
	ed composePromptWith: 'prompt'.
	rs := ed input asString readStream.
	self assert: rs next equals: Character cr.
	self assert: (rs upTo: Character cr) equals: ed separator asString.
	self assert: rs next equals: $>.
	self assert: rs next equals: Character space.
	self assert: rs upToEnd equals: 'prompt'.

]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposeResultTitle [
	ed composeResult: 'result' title: 'RES'.
	self assert: ed result asString equals: self resultTextStringExample
]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposeStackTest [
	| rs ws stack |
	ed composeStackText.
	rs := ed stackText asString readStream.
	
	self assert: rs next equals: Character cr.
	self assert: (rs upTo: Character cr) equals: ed separator asString.
	self
		assert: (rs upTo: Character cr)
		equals:
			'STACK (' , ed stackSelectionIndex printString , '/'
				, debugAPI stackSize printString , ')'.				
	self assert: (rs upTo: Character cr) equals: ed separator asString.
	
	
	stack := debugAPI stack copyFrom: 1 to: debugAPI displayStackSize.
	
	ws := WriteStream on: String new.
	(stack at: 1) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 2) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 3) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 4) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 5) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	self assert: rs atEnd
	 
]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposeStackText [
	| rs ws stack |
	ed composeStackText.
	rs := ed stackText asString readStream.
	
	self assert: rs next equals: Character cr.
	self assert: (rs upTo: Character cr) equals: ed separator asString.
	self
		assert: (rs upTo: Character cr)
		equals:
			'STACK (' , ed stackSelectionIndex printString , '/'
				, debugAPI stackSize printString , ')'.				
	self assert: (rs upTo: Character cr) equals: ed separator asString.
	
	
	stack := debugAPI stack copyFrom: 1 to: debugAPI displayStackSize.
	
	ws := WriteStream on: String new.
	(stack at: 1) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 2) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 3) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 4) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	ws := WriteStream on: String new.
	(stack at: 5) printWithArgsValueOn: ws.
	self assert: (rs upTo: Character cr) equals: ws contents.
	
	self assert: rs atEnd
	 
]

{ #category : #'text - composition' }
EDEmergencyDebuggerTest >> testComposeTitleText [
	ed composeMethodText.
	self
		assert: ed titleText
		equals: self titleDisplayTextStringExample
]

{ #category : #'debug API' }
EDEmergencyDebuggerTest >> testDebugAPI [
	|api|
	api := EDMockObjectForTests new.
	EDEmergencyDebugger debugAPI: api.
	self assert: EDEmergencyDebugger debugAPI identicalTo: api
	
]

{ #category : #'debug API' }
EDEmergencyDebuggerTest >> testDefaultDebugAPI [
	
	self assert: EDEmergencyDebugger debugAPI identicalTo: EDDebuggingAPI
	
]

{ #category : #'text - helpers' }
EDEmergencyDebuggerTest >> testDefaultResultText [

	self assert: ed defaultResultText equals: 'TYPE h FOR HELP'
]

{ #category : #setup }
EDEmergencyDebuggerTest >> testFromError [
	|error|
	error := self zeroDivideErrorInstance.
	self assert: ed originalError isNil.
	self assert: ed errorText equals: Text new.	
	ed fromError: error.
	self assert: ed originalError identicalTo: error.
	self assert: ed errorText equals: (self errorText: error)
]

{ #category : #'methods version' }
EDEmergencyDebuggerTest >> testInitialMethodVersion [
	self assert: ed methodVersionSelectionIndex equals: 0
]

{ #category : #setup }
EDEmergencyDebuggerTest >> testIsRunning [
	
	"This is a new Ed, we did not call #debug: so it is not running"
	self deny: EDEmergencyDebugger new isRunning.
	"This Ed, we called #debug: in the test setup so it is running"
	self assert: ed isRunning 
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testMoveDownInStack [ 
	|selectionIndexBefore shortStackIndexBefore|
	selectionIndexBefore := ed stackSelectionIndex.
	shortStackIndexBefore := ed shortStackIndex.
	ed moveDownInStack.
	self assert: ed stackSelectionIndex 
			equals: ((selectionIndexBefore + 1) min: debugAPI stackSize).
	self assert: ed shortStackIndex equals: (shortStackIndexBefore + 1).
	self assert: ed methodVersionSelectionIndex equals: 0		
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testMoveDownInStackFromLastElement [ 
	|selectionIndexBefore shortStackIndexBefore|
	ed stackSelectionIndex: debugAPI stackSize.
	selectionIndexBefore := ed stackSelectionIndex.
	shortStackIndexBefore := ed shortStackIndex.
	ed moveDownInStack.
	self assert: ed stackSelectionIndex equals: selectionIndexBefore.
	self assert: ed shortStackIndex equals: shortStackIndexBefore
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testMoveUpInStack [ 
	ed stackSelectionIndex: debugAPI stackSize.
	ed shortStackIndex: debugAPI stackSize.
	ed moveUpInStack.
	self assert: ed stackSelectionIndex equals: (debugAPI stackSize - 1).
	self assert: ed shortStackIndex equals: (debugAPI stackSize - 1).
	self assert: ed methodVersionSelectionIndex equals: 0		
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testMoveUpInStackFromFirstElement [ 
	|selectionIndexBefore shortStackIndexBefore|
	selectionIndexBefore := ed stackSelectionIndex.
	shortStackIndexBefore := ed shortStackIndex.
	ed moveUpInStack.
	self assert: ed stackSelectionIndex equals: selectionIndexBefore.
	self assert: ed shortStackIndex equals: shortStackIndexBefore.
	self assert: ed methodVersionSelectionIndex equals: 0		
]

{ #category : #'text - helpers' }
EDEmergencyDebuggerTest >> testNewTextStream [
	|str|
	str := ed newTextStream.
	self assert: str isStream.
	self assert: str contents equals: Text new
]

{ #category : #'methods version' }
EDEmergencyDebuggerTest >> testRevertCurrentMethodToSelectedVersion [
	|secondContext originalPC newContext| 
	secondContext := ed shortStack second.
	originalPC := secondContext pc.	
	ed stackSelectionIndex: 2.
	ed shortStackIndex: 2.
	ed methodVersionSelectionIndex: 1.
	ed revertCurrentMethodToSelectedVersion.
	newContext := ed shortStack first.
	self assert: newContext identicalTo: secondContext.
	self assert: ed stackSelectionIndex equals: 1.
	self assert: ed shortStackIndex equals: 1.
	self assert: ed methodVersionSelectionIndex equals: 1.
	self assert: newContext pc < originalPC.
	self assert: newContext method sourceCode equals: secondContext method sourceCode.
	
	"When the context is restarted, the interpreter automatically steps over
	so-called uninteresting bytecodes, i.e., bytecodes that are not sends nor returns.
	The test method, originally EDMockObjectForTests>>#m:, starts with an equality check
	that is 2 bytecodes long, then the interpreter stops on the first send.
	The PC should be equal to the method's initial PC + 2."  
	self assert: ed shortStack first pc 
	     equals: ed shortStack first method initialPC + 2
]

{ #category : #'methods version' }
EDEmergencyDebuggerTest >> testSelectedMethodVersion [

	
]

{ #category : #'methods version' }
EDEmergencyDebuggerTest >> testSelectedMethodVersionsSize [
	self prepareMethodVersionTest.
	self assert: ed selectedMethodVersionsSize equals: 2
]

{ #category : #'text - helpers' }
EDEmergencyDebuggerTest >> testSeparator [

	self assert: ed separator asString equals: '--------------------'
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testShiftDownShortStack [
	ed stackSelectionIndex: debugAPI displayStackSize.
	ed shortStackIndex: debugAPI displayStackSize.
	ed moveDownInStack.
	self assert: ed shortStackIndex equals: 1.
	self
		assertCollection: ed shortStack
		hasSameElements:
			(session stack
				copyFrom: debugAPI displayStackSize + 1
				to: debugAPI displayStackSize * 2)
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testShiftUpShortStack [
	ed stackSelectionIndex: debugAPI displayStackSize + 1.
	ed shortStackIndex: 1.
	ed moveUpInStack.
	self assert: ed shortStackIndex equals: debugAPI displayStackSize.
	self
		assertCollection: ed shortStack
		hasSameElements: (session stack copyFrom: 1 to: debugAPI displayStackSize)
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testShortStack [ 
	self assert: ed shortStack size equals: 5.	
	self assertCollection: (session stackOfSize: 5) hasSameElements: ed shortStack
]

{ #category : #'methods version' }
EDEmergencyDebuggerTest >> testShowMoreRecentMethodVersion [
	self prepareMethodVersionTest.
	ed showMoreRecentMethodVersion.
	self assert: ed methodVersionSelectionIndex equals: 1.
	ed showOlderMethodVersion.
	ed showMoreRecentMethodVersion.
	self assert: ed methodVersionSelectionIndex equals: 1
]

{ #category : #'methods version' }
EDEmergencyDebuggerTest >> testShowOlderMethodVersion [
	self prepareMethodVersionTest.
	ed showOlderMethodVersion.
	self assert: ed methodVersionSelectionIndex equals: 1.
	ed showOlderMethodVersion.
	self assert: ed methodVersionSelectionIndex equals: 2
]

{ #category : #'methods version' }
EDEmergencyDebuggerTest >> testShowSelectedMethodVersion [

	
]

{ #category : #setup }
EDEmergencyDebuggerTest >> testTerminate [
	
	ed terminate.
	self deny: ed isRunning 
]

{ #category : #updating }
EDEmergencyDebuggerTest >> testUpdateDisplay [
	ed updateDisplay.
	self assert: mockDisplayAPI isCleared.
	self assert: mockDisplayAPI displayData equals: ed composeDisplayText 
]

{ #category : #updating }
EDEmergencyDebuggerTest >> testUpdatePrompt [
	| promptUpdate rs |
	promptUpdate := 'PROMPT UPDATE'.
	ed updatePrompt: promptUpdate.
	rs := ed input readStream.
	rs upTo: $>.
	rs upTo: Character space.
	self assert: (rs upTo: Character cr) equals: promptUpdate
]

{ #category : #'api - stack' }
EDEmergencyDebuggerTest >> testUpdateShortStackForDisplay [ 
	
]

{ #category : #'text - helpers' }
EDEmergencyDebuggerTest >> testWriteSectionTitleOn [
	| expectedTextStream actualTextStream |
	
	expectedTextStream := WriteStream on: Text new.
	ed writeSeparatorOn: expectedTextStream.
	expectedTextStream << 'TEST'.
	expectedTextStream cr.
	ed writeSeparatorOn: expectedTextStream. 
	
	actualTextStream := WriteStream on: Text new.	
	ed writeSectionTitle: 'TEST' on: actualTextStream.

	self assert: actualTextStream contents asString 
		  equals: expectedTextStream contents asString
]

{ #category : #'text - helpers' }
EDEmergencyDebuggerTest >> testWriteSeparatorOn [
	| expectedTextStream actualTextStream |
	
	expectedTextStream := WriteStream on: Text new.
	expectedTextStream << ed separator.
	expectedTextStream cr.
	
	actualTextStream := WriteStream on: Text new.	
	ed writeSeparatorOn: actualTextStream.

	self assert: actualTextStream contents asString 
		  equals: expectedTextStream contents asString
]

{ #category : #helpers }
EDEmergencyDebuggerTest >> titleDisplayTextStringExample [
	^ 'Hi, I''m ED - What is the nature of your debugging emergency?'
]

{ #category : #helpers }
EDEmergencyDebuggerTest >> zeroDivideErrorInstance [
	[ 1 / 0 ]
		on: Error
		do: [ :err | ^ err ]
]
