Class {
	#name : #StObjectBreakpointInspectionTest,
	#superclass : #TestCase,
	#instVars : [
		'breakpoint1',
		'breakpoint2',
		'instance',
		'inspection',
		'breakpoint3',
		'breakpoint4'
	],
	#category : #'NewTools-Debugger-Breakpoints-Tools-Tests'
}

{ #category : #helpers }
StObjectBreakpointInspectionTest >> compileHelperMethods [

	StDummyTestClassWithHalts compile: 'varRead ^var'.
	StDummyTestClassWithHalts compile: 'varWrite var := 0'
]

{ #category : #helpers }
StObjectBreakpointInspectionTest >> removeHelperMethods [

	StDummyTestClassWithHalts removeSelector: #varRead.
	StDummyTestClassWithHalts removeSelector: #varWrite
]

{ #category : #running }
StObjectBreakpointInspectionTest >> setUp [
	super setUp.
	self removeHelperMethods.
	self compileHelperMethods.
	
	breakpoint1 := Breakpoint new node: (StDummyTestClassWithHalts>>#varRead) ast.
	breakpoint1  install.
	
	breakpoint2  := VariableBreakpoint watchVariable: #var inClass: StDummyTestClassWithHalts.
	
	instance := StDummyTestClassWithHalts new.	
	breakpoint3 := VariableBreakpoint watchVariable: #var inObject: instance.
	breakpoint4 := instance haltOnCallTo: #varRead.	
	
	
]

{ #category : #running }
StObjectBreakpointInspectionTest >> tearDown [
	breakpoint1 ifNotNil: [ breakpoint1 remove ].
	breakpoint2 ifNotNil: [ breakpoint2 remove ].
	breakpoint3 ifNotNil: [ breakpoint3 remove ].
	breakpoint4 ifNotNil: [ breakpoint4 remove ].
	self removeHelperMethods.
	super tearDown
]

{ #category : #tests }
StObjectBreakpointInspectionTest >> testAllBreakpointsForModel [

	inspection := StObjectBreakpointInspection basicNew model: instance.
	self assertCollection: inspection breakpointsForModel equals: { 
			breakpoint1.
			breakpoint2.
			breakpoint3.
			breakpoint4 } asOrderedCollection
]

{ #category : #tests }
StObjectBreakpointInspectionTest >> testBreakpointsForModel [

	inspection := StObjectBreakpointInspection basicNew model:
		              StDummyTestClassWithHalts new.
	self assertCollection: inspection breakpointsForModel equals: { 
			breakpoint1.
			breakpoint2 } asOrderedCollection
]

{ #category : #tests }
StObjectBreakpointInspectionTest >> testObjectCentricBreakpointsForModel [

	breakpoint1 remove.
	breakpoint1 := nil.
	breakpoint2 remove.
	breakpoint2 := nil.
	inspection := StObjectBreakpointInspection basicNew model:
		              instance.
	self assertCollection: inspection breakpointsForModel equals: { 
			breakpoint3.
			breakpoint4. } asOrderedCollection
]
