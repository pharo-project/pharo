"
A BreakpointTest is a test class for testing the behavior of Breakpoint
"
Class {
	#name : 'BreakpointTest',
	#superclass : 'TestCase',
	#instVars : [
		'previousBreakpoints',
		'cls',
		'oldSystemAnnouncer'
	],
	#category : 'Reflectivity-Tools-Tests',
	#package : 'Reflectivity-Tools-Tests'
}

{ #category : 'utilities' }
BreakpointTest >> newDummyClass [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: #DummyClassForBreakpoint;
			  package: 'DummyPackage' ]
]

{ #category : 'tests' }
BreakpointTest >> parserClass [
	^ RBParser
]

{ #category : 'running' }
BreakpointTest >> setUp [
	super setUp.
	cls := self newDummyClass.
	previousBreakpoints := Breakpoint all copy.
	oldSystemAnnouncer := self class codeSupportAnnouncer.
	self class environment codeSupportAnnouncer: nil.
	Breakpoint registerInterestToSystemAnnouncement.
	Breakpoint all removeAll
]

{ #category : 'running' }
BreakpointTest >> tearDown [

	Breakpoint removeAll.
	Breakpoint all addAll: previousBreakpoints.
	self class environment codeSupportAnnouncer: oldSystemAnnouncer.
	cls ifNotNil: [ cls isObsolete ifFalse: [ cls removeFromSystem ] ].
	self packageOrganizer removePackage: 'DummyPackage'.
	super tearDown
]

{ #category : 'tests' }
BreakpointTest >> testAddRemoveBreakpoint [
	| bp method |
	cls compile: 'dummy ^42'.
	method := cls >> #dummy.
	self assertEmpty: Breakpoint all.
	bp := Breakpoint new.
	bp	node: (cls >> #dummy) ast.
	bp install.
	"after the breakpoint is installed, the method is different"
	self deny: (cls >> #dummy) bytecodes equals: method bytecodes.

	self assertCollection: Breakpoint all includesAll: {bp}.
	bp remove.
	self assertEmpty: Breakpoint all.

	"Check that the method is correcty reverted"
	self assert: (cls >> #dummy) bytecodes equals: method bytecodes
]

{ #category : 'tests' }
BreakpointTest >> testAddRemoveBreakpointKeepTimeStamp [

	| bp timeStamp |
	"adding an removing a breakpoint does not change the timestamp of the method"
	cls compile: 'dummy ^42'.

	timeStamp := (cls >> #dummy) timeStamp.

	bp := Breakpoint new.
	bp node: (cls >> #dummy) ast.
	bp install.

	self assert: (cls >> #dummy) timeStamp equals: timeStamp.

	bp remove.
	self assertEmpty: Breakpoint all.

	self assert: (cls >> #dummy) timeStamp equals: timeStamp
]

{ #category : 'tests' }
BreakpointTest >> testBreakInContextNode [

	|observer breakpoint|
	observer := DummyBreakpointObserver new.
	Breakpoint registerObserver: observer.
	breakpoint := Breakpoint new.
	self should: [breakpoint breakInContext: self node: #node] raise: Break.
	self assert: observer tag isNotNil.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: #(#node)
]

{ #category : 'tests' }
BreakpointTest >> testBreakLink [
	|breakpoint link|
	breakpoint := Breakpoint new.
	link := breakpoint link.
	self assert: link metaObject identicalTo: breakpoint.
	self assert: link selector equals: #breakInContext:node:.
	self assertCollection: link options equals: (MetaLink new parseOptions: breakpoint options) options.
	self assertCollection: link arguments equals: #(#context #node)
]

{ #category : 'tests' }
BreakpointTest >> testBreakLinkOnce [
	|breakpoint|
	breakpoint := Breakpoint new.
	self deny: (breakpoint once).
	breakpoint once: true.
	self assert: (breakpoint once)
]

{ #category : 'tests' }
BreakpointTest >> testBreakOnceInContext [

	|observer breakpoint|
	observer := DummyBreakpointObserver new.
	Breakpoint registerObserver: observer.
	breakpoint := Breakpoint new once: true.
	self assert: breakpoint isEnabled.
	self should: [breakpoint breakInContext: self node: #node] raise: Break.
	self deny: breakpoint isEnabled.
	self shouldnt: [breakpoint breakInContext: self node: #node] raise: Break
]

{ #category : 'tests' }
BreakpointTest >> testBreakpointEnabled [
	| breakpoint |
	breakpoint := Breakpoint new.
	self assert: breakpoint isEnabled.
	breakpoint disable.
	self deny: breakpoint isEnabled.
	breakpoint enable.
	self assert: breakpoint isEnabled
]

{ #category : 'tests' }
BreakpointTest >> testBreakpointInitialization [
	|bp|
	bp := Breakpoint new.
	self assert: bp targetInstance isNil.
	self deny: bp isObjectCentric
]

{ #category : 'tests' }
BreakpointTest >> testBreakpointNodeProperty [
	|breakpoint|
	cls compile: 'dummy ^42'.
	breakpoint := Breakpoint new
		node: (cls >> #dummy) ast;
		once: true.
	breakpoint install.
	self assert: (cls >> #dummy) ast hasBreakpoint.
	self assertCollection: (cls >> #dummy) ast breakpoints equals: {breakpoint} asSet.
	breakpoint remove.
	self deny: (cls >> #dummy) ast hasBreakpoint
]

{ #category : 'tests' }
BreakpointTest >> testDisableEnableBreakpoint [
	| breakpoint |
	breakpoint := Breakpoint new.
	self should: [breakpoint breakInContext: self node: #node] raise: Break.
	breakpoint disable.
	self shouldnt: [breakpoint breakInContext: self node: #node] raise: Break.
	breakpoint enable.
	self should: [breakpoint breakInContext: self node: #node] raise: Break
]

{ #category : 'tests' }
BreakpointTest >> testMessageSend_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'val msg: arg')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ((ThisContext readVariableNamed: #val) msg: (ThisContext readVariableNamed: #arg))]')
]

{ #category : 'tests' }
BreakpointTest >> testModifyMethodWithBreakpoint [
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once: true;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls compile: 'dummy ^43'.
	self assertEmpty: Breakpoint all
]

{ #category : 'tests' }
BreakpointTest >> testObjectCentricAPI [
	|object1 object2|
	object1 := ReflectivityExamples new.
	object2 := ReflectivityExamples new.

	Breakpoint breakOn: #exampleAssignment inObject: object1.
	self should: [object1 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break.
	
	Breakpoint breakOnceOn: #exampleAssignment inObject: object2.
	self should: [object2 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break
]

{ #category : 'tests' }
BreakpointTest >> testObjectCentricASTAPI [
	|object1 object2 ast|
	object1 := ReflectivityExamples new.
	object2 := ReflectivityExamples new.
	ast := (ReflectivityExamples >> #exampleAssignment) ast statements first.

	Breakpoint breakOnAST: ast inObject: object1.
	self should: [object1 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break.	

	Breakpoint breakOnceOnAST: ast inObject: object2.
	self should: [object2 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break
]

{ #category : 'tests' }
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
	self shouldnt: [object1 exampleAssignment] raise: Break
]

{ #category : 'tests' }
BreakpointTest >> testObjectCentricObjectAPI [
	|object1 object2|
	object1 := ReflectivityExamples new.
	object2 := ReflectivityExamples new.

	object1 haltOnCallTo: #exampleAssignment.
	self should: [object1 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break.
	
	object2 haltOnceOnCallTo: #exampleAssignment.
	self should: [object2 exampleAssignment] raise: Break.
	self shouldnt: [object2 exampleAssignment] raise: Break
]

{ #category : 'tests' }
BreakpointTest >> testOnceBreakpointDisableAfterHit [
	|object breakpoint|
	object := ReflectivityExamples new.
	
	breakpoint := Breakpoint breakOnceOn: #exampleAssignment inObject: object.
	breakpoint disable.
	self shouldnt: [object exampleAssignment] raise: Break.
	breakpoint enable.
	self should: [object exampleAssignment] raise: Break.
	self deny: breakpoint isEnabled
]

{ #category : 'tests' }
BreakpointTest >> testRemoveBreakpointWithRemoveFrom [
	|bp node |
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.

	node := (cls >> #dummy) ast.
	bp := (Breakpoint new node: node) install.
	self assertCollection: Breakpoint all includesAll: {bp}.
	Breakpoint removeFrom: node.
	self assertEmpty: Breakpoint all
]

{ #category : 'tests' }
BreakpointTest >> testRemoveClassWithBreakpoint [
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once: true;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls removeFromSystem.
	self assertEmpty: Breakpoint all
]

{ #category : 'tests' }
BreakpointTest >> testRemoveMethodWithBreakpoint [
	cls compile: 'dummy ^42'.
	self assertEmpty: Breakpoint all.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once: true;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls removeSelector: #dummy.
	self
		assertEmpty: cls methods;
		assertEmpty: Breakpoint all
]

{ #category : 'tests' }
BreakpointTest >> testScopeTo [
	|bp object|
	bp := Breakpoint new.
	object := Object new.
	bp scopeTo: object.

	self assert: bp targetInstance identicalTo: object.
	self assert: bp isObjectCentric
]

{ #category : 'tests' }
BreakpointTest >> testSelf_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'self')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext receiver ]')
]

{ #category : 'tests' }
BreakpointTest >> testSetAsBreakpointProperty [
	| node breakpoint breakpoint2 |
	node := ASTProgramNode new.
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

{ #category : 'tests' }
BreakpointTest >> testSimpleCondition_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'true')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: true ]')
]

{ #category : 'tests' }
BreakpointTest >> testSuperSendNoArg_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'super msg')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #msg withArguments: {} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : 'tests' }
BreakpointTest >> testSuperSendWithArgs_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'super foo: arg1 bar: arg2')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #foo:bar: withArguments: {ThisContext readVariableNamed: #arg1. ThisContext readVariableNamed: #arg2.} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

	self shouldnt: [ bp remove ] raise: Error
]

{ #category : 'tests' }
BreakpointTest >> testUpdateMethod [
	| bp method |
	"test that, if we install a breakpoint and edit the method, it is removed correctly"

	cls compile: 'dummy ^42'.
	method := cls >> #dummy.
	self assertEmpty: Breakpoint all.
	bp := Breakpoint new.
	bp	node: (cls >> #dummy) ast.
	bp install.
	self assertCollection: Breakpoint all includesAll: {bp}.
	"no edit the method"
	cls compile: 'dummy ^nil'.
	"after the new method is installed, the method is different"
	self deny: (cls >> #dummy) bytecodes equals: method bytecodes.
	"and the breakpoint is not registered anymore"
	self assertEmpty: Breakpoint all
]

{ #category : 'tests' }
BreakpointTest >> testVariableLookup_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'var')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext readVariableNamed: #var)]')
]

{ #category : 'tests' }
BreakpointTest >> testVariableNamedThisContext_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'ThisContext')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext readVariableNamed: #ThisContext)]')
]

{ #category : 'tests' }
BreakpointTest >> testthisContext_conditionBlockProducerMethodAST [
	self
		assert:
			(Breakpoint new
				conditionBlockProducerMethodAST: 'thisContext')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext ]')
]
