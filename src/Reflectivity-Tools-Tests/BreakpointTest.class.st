"
A BreakpointTest is a test class for testing the behavior of Breakpoint
"
Class {
	#name : #BreakpointTest,
	#superclass : #TestCase,
	#instVars : [
		'previousBreakpoints',
		'cls'
	],
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #utilities }
BreakpointTest >> newDummyClass [
	^ Object
		subclass: #DummyClassForBreakpoint
		instanceVariableNames: ''
		classVariableNames: ''
		package: 'DummyPackage'
]

{ #category : #running }
BreakpointTest >> setUp [
	super setUp.
	cls := self newDummyClass.
	previousBreakpoints := Breakpoint all copy.
	Breakpoint all removeAll.
]

{ #category : #running }
BreakpointTest >> tearDown [
	| pkg |
	Breakpoint removeAll.	
	Breakpoint all addAll: previousBreakpoints.
	cls ifNotNil: [ cls isObsolete ifFalse: [ cls removeFromSystem ] ].
	pkg := 'DummyPackage' asPackageIfAbsent: [ ]. 
	pkg ifNotNil: [ pkg removeFromSystem ].
	super tearDown.
]

{ #category : #tests }
BreakpointTest >> testAddRemoveBreakpoint [
	|bp|
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.
	bp := Breakpoint new.
	bp	node: (cls >> #dummy) ast.
	Breakpoint addBreakpoint: bp.
	self assertCollection: Breakpoint all includesAll: {bp}.
	Breakpoint removeBreakpoint: bp.
	self assertEmpty: Breakpoint all.
]

{ #category : #tests }
BreakpointTest >> testBreakpointInitialization [
	|bp|	
	bp := Breakpoint new.
	self assert: bp targetInstance isNil.
	self deny: bp isObjectCentric
]

{ #category : #tests }
BreakpointTest >> testModifyMethodWithBreakpoint [
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls compile: 'dummy ^43'.
	self assertEmpty: Breakpoint all
]

{ #category : #tests }
BreakpointTest >> testObjectCentricBreakpoint [
	|bp object1 object2 ast|	
	bp := Breakpoint new.
	object1 := ReflectivityExamples new.
	object2 := ReflectivityExamples new.
	ast := (ReflectivityExamples >> #exampleAssignment) ast.
	
	bp node: ast.
	bp scopeTo: object1.
	bp install.
	
	self should: [object1 exampleAssignment] raise: Halt.
	self shouldnt: [object2 exampleAssignment] raise: Halt
]

{ #category : #tests }
BreakpointTest >> testRemoveClassWithBreakpoint [
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls removeFromSystem.
	self assertEmpty: Breakpoint all
]

{ #category : #tests }
BreakpointTest >> testRemoveMethodWithBreakpoint [
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls removeSelector: #dummy.
	self
		assertEmpty: cls methods;
		assertEmpty: Breakpoint all
]

{ #category : #tests }
BreakpointTest >> testScopeTo [
	|bp object|	
	bp := Breakpoint new.
	object := Object new.
	bp scopeTo: object.
	
	self assert: bp targetInstance identicalTo: object.
	self assert: bp isObjectCentric
]

{ #category : #tests }
BreakpointTest >> testTargetInstanceGarbageCollected [
	|bp object|	
	bp := Breakpoint new.
	object := Object new.
	bp scopeTo: object.
	object := nil.
	Smalltalk garbageCollect.
	
	self assert: bp targetInstance isNil.
	self deny: bp isObjectCentric
]

{ #category : #tests }
BreakpointTest >> testUninstallFromGarbageCollectedInstance [
	|bp object ast|	
	bp := Breakpoint new.
	object := ReflectivityExamples new.
	ast := (ReflectivityExamples >> #exampleAssignment) ast.
	
	bp node: ast.
	bp scopeTo: object.
	bp install.
	
	object := nil.
	Smalltalk garbageCollect.
	
	self shouldnt: [ bp remove ] raise: Error.
]
