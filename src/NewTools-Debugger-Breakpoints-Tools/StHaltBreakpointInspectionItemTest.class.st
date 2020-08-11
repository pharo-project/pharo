Class {
	#name : #StHaltBreakpointInspectionItemTest,
	#superclass : #TestCase,
	#category : #'NewTools-Debugger-Breakpoints-Tools-Tests'
}

{ #category : #helpers }
StHaltBreakpointInspectionItemTest >> breakpoint [

	^ Breakpoint new node:
		  (StDummyTestClassWithHalts >> #mHalt) ast statements first
]

{ #category : #helpers }
StHaltBreakpointInspectionItemTest >> compileHaltNode [

	StDummyTestClassWithHalts compile: 'mHalt self halt'.
]

{ #category : #helpers }
StHaltBreakpointInspectionItemTest >> haltNode [

	^ (StDummyTestClassWithHalts >> #mHalt) ast statements first
]

{ #category : #helpers }
StHaltBreakpointInspectionItemTest >> removeHaltNode [

	StDummyTestClassWithHalts removeSelector: #mHalt
]

{ #category : #running }
StHaltBreakpointInspectionItemTest >> setUp [
	super setUp.
	StHaltCache defaultCache removeSubscriptions.	
	self compileHaltNode.
]

{ #category : #running }
StHaltBreakpointInspectionItemTest >> tearDown [

	self removeHaltNode.
	StHaltCache defaultCache updateSubscriptions.
	super tearDown
]

{ #category : #'tests - initialization' }
StHaltBreakpointInspectionItemTest >> testNewBreakpointItem [
	|bp item|
	bp := self breakpoint.
	item := StHaltInspectionItem forBreakpoint: bp.
	self assert: item breakpoint identicalTo: bp. 
	self assert: item node identicalTo: bp node.
	self assert: item interval equals: (bp node start to: bp node stop).
	self assert: item scope identicalTo: bp node methodNode method methodClass.
	self assert: item displayEntity identicalTo: bp.
	self assert: item type equals: Break printString
]

{ #category : #'tests - initialization' }
StHaltBreakpointInspectionItemTest >> testNewHaltItem [
	|item|
	item := StHaltInspectionItem forHalt: self haltNode.
	self assert: item node identicalTo: self haltNode.
	self assert: item interval equals: (self haltNode start to: self haltNode stop).
	self assert: item scope identicalTo: self haltNode methodNode method methodClass.
	self assert: item displayEntity identicalTo: self haltNode methodNode.
	self assert: item type equals: Halt name asString
]

{ #category : #'tests - initialization' }
StHaltBreakpointInspectionItemTest >> testNewObjectCentricBreakpointItem [
	|bp item|
	bp := self breakpoint.
	bp scopeTo: self.
	item := StHaltInspectionItem forBreakpoint: bp.
	self assert: item scope identicalTo: self
]

{ #category : #'tests - initialization' }
StHaltBreakpointInspectionItemTest >> testNewObjectCentricVariableBreakpointItem [
	|bp item|
	bp := self variableBreakpoint.
	bp scopeTo: self.
	item := StHaltInspectionItem forBreakpoint: bp.
	self assert: item scope identicalTo: self
]

{ #category : #'tests - initialization' }
StHaltBreakpointInspectionItemTest >> testNewVariableBreakpointItem [
	|bp item|
	bp := self variableBreakpoint.
	item := StHaltInspectionItem forBreakpoint: bp.
	self assert: item breakpoint identicalTo: bp. 
	self assert: item node isNil.
	self assert: item interval isNil.
	self assert: item scope identicalTo: bp targetClassOrMethod.
	self assertCollection: item displayEntity equals: bp.
	self assert: item type equals: Break printString
]

{ #category : #'tests - printing' }
StHaltBreakpointInspectionItemTest >> testPrintBreakpointItem [

	|bp item|
	bp := self breakpoint.
	item := StHaltInspectionItem forBreakpoint: bp.
	self assert: item printContents equals: bp node methodNode sourceCode.
	self assert: item printEntity equals: bp printTargetForBreakpointInspection.
	self assert: item printScope equals: bp node methodNode methodClass printString.
	
	self flag: 'This is not super nice...'.
	"If the breakpoint is scoped to an object, it should show 'self' in that object's inspector.
	This has no meaning outside the inspected object."	
	bp scopeTo: Object new.
	self assert: item printScope equals: 'self'.
]

{ #category : #'tests - printing' }
StHaltBreakpointInspectionItemTest >> testPrintHaltItem [

	|item|
	item := StHaltInspectionItem forHalt: self haltNode.
	self assert: item printContents equals: self haltNode methodNode sourceCode.
	self assert: item printEntity equals: self haltNode methodNode method printString.
	self assert: item printScope equals: self haltNode methodNode methodClass printString
]

{ #category : #'tests - printing' }
StHaltBreakpointInspectionItemTest >> testPrintVariableBreakpointItem [

	| bp item |
	bp := self variableBreakpoint.
	item := StHaltInspectionItem forBreakpoint: bp.
	self assert: item printContents equals: bp printContentsForBreakpointInspection.
	self assert: item printEntity equals: bp printTargetForBreakpointInspection.
	self assert: item printScope equals: bp targetClassOrMethod printString.
	
	self flag: 'This is not super nice...'.
	"If the breakpoint is scoped to an object, it should show 'self' in that object's inspector.
	This has no meaning outside the inspected object."	
	bp scopeTo: Object new.
	self assert: item printScope equals: 'self'.
]

{ #category : #helpers }
StHaltBreakpointInspectionItemTest >> variableBreakpoint [

	^ StDummyTestClassWithHalts newBreakpointForVariable: #var
]
