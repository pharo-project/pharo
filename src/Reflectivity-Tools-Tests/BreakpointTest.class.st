"
A BreakpointTest is a test class for testing the behavior of Breakpoint
"
Class {
	#name : #BreakpointTest,
	#superclass : #TestCase,
	#instVars : [
		'previousBreakpoints',
		'cls',
		'oldObservers'
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
	oldObservers := Breakpoint observers copy.
	Breakpoint observers removeAll.
	Breakpoint all removeAll.
]

{ #category : #running }
BreakpointTest >> tearDown [
	| pkg |
	Breakpoint removeAll.	
	Breakpoint all addAll: previousBreakpoints.
	Breakpoint observers removeAll.
	Breakpoint observers addAll: oldObservers.
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
BreakpointTest >> testBreakInContextNode [

	|observer breakpoint|
	observer := DummyBreakpointObserver new.
	Breakpoint registerObserver: observer.
	breakpoint := Breakpoint new.	
	self should: [breakpoint breakInContext: self node: #node] raise: Break.
	self assert: observer tag notNil.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: #(#node).
	
]

{ #category : #tests }
BreakpointTest >> testBreakLink [
	|breakpoint link|
	breakpoint := Breakpoint new.
	link := breakpoint link.
	self assert: link metaObject identicalTo: breakpoint.
	self assert: link selector equals: #breakInContext:node:.
	self assertCollection: link options equals: (MetaLink new parseOptions: breakpoint options) options.
	self assertCollection: link arguments equals: #(#context #node) 
]

{ #category : #tests }
BreakpointTest >> testBreakLinkOnce [
	|breakpoint|
	breakpoint := Breakpoint new.
	self deny: breakpoint oneShot.
	breakpoint once.
	self assert: breakpoint oneShot
]

{ #category : #tests }
BreakpointTest >> testBreakOnceInContext [

	|observer breakpoint|
	observer := DummyBreakpointObserver new.
	Breakpoint registerObserver: observer.
	breakpoint := Breakpoint new once.	
	self assert: breakpoint isEnabled.
	self should: [breakpoint breakInContext: self node: #node] raise: Break.
	self deny: breakpoint isEnabled.
	self shouldnt: [breakpoint breakInContext: self node: #node] raise: Break.
]

{ #category : #tests }
BreakpointTest >> testBreakpointEnabled [
	| breakpoint |
	breakpoint := Breakpoint new.
	self assert: breakpoint isEnabled.
	breakpoint disable.
	self deny: breakpoint isEnabled.
	breakpoint enable.
	self assert: breakpoint isEnabled
]

{ #category : #tests }
BreakpointTest >> testBreakpointInitialization [
	|bp|	
	bp := Breakpoint new.
	self assert: bp targetInstance isNil.
	self deny: bp isObjectCentric
]

{ #category : #tests }
BreakpointTest >> testBreakpointNodeProperty [
	|breakpoint|
	cls compile: 'dummy ^42'.
	breakpoint := Breakpoint new
		node: (cls >> #dummy) ast;
		once.
	breakpoint install.
	self assert: (cls >> #dummy) ast hasBreakpoint.
	self assertCollection: (cls >> #dummy) ast breakpoints equals: {breakpoint} asSet.
	breakpoint remove.
	self deny: (cls >> #dummy) ast hasBreakpoint.
]

{ #category : #tests }
BreakpointTest >> testDisableEnableBreakpoint [
	| breakpoint |
	breakpoint := Breakpoint new.	
	self should: [breakpoint breakInContext: self node: #node] raise: Break.
	breakpoint disable.
	self shouldnt: [breakpoint breakInContext: self node: #node] raise: Break.
	breakpoint enable.
	self should: [breakpoint breakInContext: self node: #node] raise: Break

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
BreakpointTest >> testObjectCentricAPI [
	|object1 object2|		
	object1 := ReflectivityExamples new.
	object2 := ReflectivityExamples new.
	
	Breakpoint breakOn: #exampleAssignment inObject: object1.
	
	self should: [object1 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break
]

{ #category : #tests }
BreakpointTest >> testObjectCentricASTAPI [
	|object1 object2 ast|		
	object1 := ReflectivityExamples new.
	object2 := ReflectivityExamples new.
	ast := (ReflectivityExamples >> #exampleAssignment) ast statements first.
	
	Breakpoint breakOnAST: ast inObject: object1.
	
	self should: [object1 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break
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
	
	self should: [object1 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break.
	
	bp remove.
	self shouldnt: [object1 exampleAssignment] raise: Break.
]

{ #category : #tests }
BreakpointTest >> testObjectCentricObjectAPI [
	|object1 object2|		
	object1 := ReflectivityExamples new.
	object2 := ReflectivityExamples new.
	
	object1 haltOnCallTo: #exampleAssignment.
	
	self should: [object1 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break
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
BreakpointTest >> testSetAsBreakpointProperty [
	| node breakpoint breakpoint2 |
	node := RBProgramNode new.
	breakpoint := Breakpoint new.
	breakpoint2 := Breakpoint new.
	breakpoint node: node.
	breakpoint2 node: node.
	
	breakpoint setAsNodeProperty.
	breakpoint2 setAsNodeProperty.
	self assert: node hasBreakpoint.
	self assertCollection: node breakpoints equals: { breakpoint. breakpoint2 } asSet.
	
	breakpoint setAsNodeProperty.
	self assertCollection: node breakpoints equals: { breakpoint. breakpoint2 } asSet.
	
	breakpoint removeFromNodeProperty.
	breakpoint2 removeFromNodeProperty.
	self deny: node hasBreakpoint
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
