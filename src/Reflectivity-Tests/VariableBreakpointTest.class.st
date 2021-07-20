Class {
	#name : #VariableBreakpointTest,
	#superclass : #TestCase,
	#instVars : [
		'obj1',
		'obj2',
		'wp',
		'obj3',
		'obj4',
		'observer',
		'testClass',
		'testSubclass'
	],
	#category : #'Reflectivity-Tests-Breakpoints'
}

{ #category : #helpers }
VariableBreakpointTest >> allNodes [
	^ OrderedCollection new
		addAll: self allNodesForV1;
		addAll: self allNodesForV2;
		yourself
]

{ #category : #helpers }
VariableBreakpointTest >> allNodesForV1 [
	^ OrderedCollection new
		addAll: self nodesForV1InFieldWatchpointMockClass;
		addAll: self nodesForV1InVariableBreakpointMockSubclass;
		yourself
]

{ #category : #helpers }
VariableBreakpointTest >> allNodesForV2 [
	^ OrderedCollection new
		addAll: self nodesForV2InVariableBreakpointMockSubclass;
		yourself
]

{ #category : #helpers }
VariableBreakpointTest >> allTempNodes [
	^ OrderedCollection new
		addAll: self tempReadNodes;
		addAll: self tempWriteNodes;
		yourself
]

{ #category : #helpers }
VariableBreakpointTest >> argNodes [
	|nodes |
	nodes := OrderedCollection new.
	nodes add: (VariableBreakpointMockClass>>#methodWithTempsAndArg:) ast statements first value.
	^nodes
]

{ #category : #helpers }
VariableBreakpointTest >> compileTestClass [

	testClass := VariableBreakpointMockClass
		             subclass: #VariableBreakpointTestSubclass
		             instanceVariableNames: 'v3'
		             classVariableNames: ''
		             package: 'Reflectivity-Tests-Breakpoints'.
	testClass compile: 'accessingV1 ^v1'.
	testClass compile: 'accessingtemp |temp| temp := 0'.
]

{ #category : #helpers }
VariableBreakpointTest >> compileTestClass2 [

	testSubclass := testClass
		                subclass: #VariableBreakpointTestSubclass2
		                instanceVariableNames: ''
		                classVariableNames: ''
		                package: 'Reflectivity-Tests-Breakpoints'.
	testSubclass compile: 'accessingV1 ^v1'
]

{ #category : #helpers }
VariableBreakpointTest >> nodesForV1InFieldWatchpointMockClass [
	|nodes |
	nodes := OrderedCollection new.
	nodes add: (VariableBreakpointMockClass>>#initialize) ast statements first.
	nodes add: (VariableBreakpointMockClass>>#v1) ast statements first value.
	nodes add: (VariableBreakpointMockClass>>#v1:) ast statements first.
	^nodes
]

{ #category : #helpers }
VariableBreakpointTest >> nodesForV1InVariableBreakpointMockSubclass [
	|nodes |
	nodes := OrderedCollection new.
	nodes add: (VariableBreakpointMockSubclass>>#instVarWrites) ast statements first.
	nodes add: (VariableBreakpointMockSubclass>>#instVarReads) ast statements first value receiver.
	^nodes
]

{ #category : #helpers }
VariableBreakpointTest >> nodesForV2InVariableBreakpointMockSubclass [
	|nodes |
	nodes := OrderedCollection new.
	nodes add: (VariableBreakpointMockSubclass>>#initialize) ast statements second.
	nodes add: (VariableBreakpointMockSubclass>>#instVarReads) ast statements first value arguments first.
	nodes add: (VariableBreakpointMockSubclass>>#instVarWrites) ast statements second.
	nodes add: (VariableBreakpointMockSubclass>>#v2) ast statements first value.
	nodes add: (VariableBreakpointMockSubclass>>#v2:) ast statements first.
	^nodes
]

{ #category : #running }
VariableBreakpointTest >> setUp [
	super setUp.	
	obj1 := VariableBreakpointMockClass new.
	obj2 := VariableBreakpointMockSubclass new.
	obj3 := VariableBreakpointMockClass new.
	obj4 := VariableBreakpointMockSubclass new.
	observer := DummyBreakpointObserver new.
	Breakpoint registerObserver: observer
]

{ #category : #running }
VariableBreakpointTest >> tearDown [
	wp
		ifNotNil: [ wp isInstalled
				ifTrue: [ wp remove ] ].
	Breakpoint unregisterObserver: observer.
	testSubclass ifNotNil:[testSubclass removeFromSystem].
	testClass ifNotNil:[testClass removeFromSystem].
	super tearDown
]

{ #category : #helpers }
VariableBreakpointTest >> tempReadNodes [
	|nodes |
	nodes := OrderedCollection new.
	nodes add: (VariableBreakpointMockClass>>#methodWithTempsAndArg:) ast statements last value.
	^nodes
]

{ #category : #helpers }
VariableBreakpointTest >> tempWriteNodes [
	|nodes |
	nodes := OrderedCollection new.
	nodes add: (VariableBreakpointMockClass>>#methodWithTempsAndArg:) ast statements first.
	^nodes
]

{ #category : #'tests - installation' }
VariableBreakpointTest >> testAccessStrategy [
	
	wp := VariableBreakpoint watchVariablesInClass: VariableBreakpointMockClass.
	self assert: wp accessStrategy equals: #all.
	wp remove.
	
	wp := VariableBreakpoint watchVariable: #v1 inClass: VariableBreakpointMockClass.
	self assert: wp accessStrategy equals: #all.
	wp remove.
	
	wp := VariableBreakpoint watchVariable: #v1 inObject: obj1.
	self assert: wp accessStrategy equals: #all.
	wp remove.
	
	wp := VariableBreakpoint watchVariablesInObject: obj1.	
	self assert: wp accessStrategy equals: #all.
	wp remove.
		
	wp := VariableBreakpoint watchVariableWrites: #v1 inClass: VariableBreakpointMockClass.
	self assert: wp accessStrategy equals: #write.
	wp remove.
	
	wp := VariableBreakpoint watchVariablesWritesInClass: VariableBreakpointMockClass.
	self assert: wp accessStrategy equals: #write.
	wp remove.
	
	wp := VariableBreakpoint watchVariablesWritesInClass: VariableBreakpointMockSubclass.
	self assert: wp accessStrategy equals: #write.
	wp remove.
	
	wp := VariableBreakpoint watchVariableWrites: #v1 inObject: obj1.
	self assert: wp accessStrategy equals: #write.
	wp remove.
	
	wp := VariableBreakpoint watchVariablesReadsInObject: obj1.
	self assert: wp accessStrategy equals: #read.
	wp remove.
	
	wp := VariableBreakpoint watchVariableReads: #v1 inClass: VariableBreakpointMockClass.
	self assert: wp accessStrategy equals: #read.
	wp remove.
	
	wp := VariableBreakpoint watchVariableReads: #v1 inObject: obj1.
	self assert: wp accessStrategy equals: #read.
	wp remove.
	
	wp := VariableBreakpoint watchVariablesReadsInObject: obj1.
	self assert: wp accessStrategy equals: #read.
	wp remove.
	
]

{ #category : #'tests - initialization' }
VariableBreakpointTest >> testAllSlotNamesFor [
	self
		assertCollection: (VariableBreakpoint allSlotNamesFor: VariableBreakpointMockSubclass)
		equals: (OrderedCollection with: #v1 with: #v2)
]

{ #category : #'tests - object-centric api' }
VariableBreakpointTest >> testBreakOnAccess [
	wp := obj2 haltOnAccess.
	self should: [ obj2 v1 ] raise: Break.
	self should: [ obj2 v1: 0 ] raise: Break.
	self should: [ obj2 v2 ] raise: Break.
	self should: [ obj2 v2: 0 ] raise: Break.
	self shouldnt: [ obj4 v1 ] raise: Break.
	self shouldnt: [ obj4 v1: 0 ] raise: Break.
	self shouldnt: [ obj4 v2 ] raise: Break.
	self shouldnt: [ obj4 v2: 0 ] raise: Break.
]

{ #category : #'tests - object-centric api' }
VariableBreakpointTest >> testBreakOnAccessTo [
	wp := obj2 haltOnAccessTo: #v1.
	self should: [ obj2 v1 ] raise: Break.
	self should: [ obj2 v1: 0 ] raise: Break.
	self shouldnt: [ obj2 v2 ] raise: Break.
	self shouldnt: [ obj2 v2: 0 ] raise: Break.
	self shouldnt: [ obj4 v1 ] raise: Break.
	self shouldnt: [ obj4 v1: 0 ] raise: Break.
	self shouldnt: [ obj4 v2 ] raise: Break.
	self shouldnt: [ obj4 v2: 0 ] raise: Break.
]

{ #category : #'tests - object-centric api' }
VariableBreakpointTest >> testBreakOnRead [
	wp := obj2 haltOnRead.
	self should: [ obj2 v1 ] raise: Break.
	self shouldnt: [ obj2 v1: 0 ] raise: Break.
	self should: [ obj2 v2 ] raise: Break.
	self shouldnt: [ obj2 v2: 0 ] raise: Break.
	self shouldnt: [ obj4 v1 ] raise: Break.
	self shouldnt: [ obj4 v1: 0 ] raise: Break.
	self shouldnt: [ obj4 v2 ] raise: Break.
	self shouldnt: [ obj4 v2: 0 ] raise: Break.
]

{ #category : #'tests - object-centric api' }
VariableBreakpointTest >> testBreakOnReadTo [
	wp := obj2 haltOnReadTo: #v1.
	self should: [ obj2 v1 ] raise: Break.
	self shouldnt: [ obj2 v1: 0 ] raise: Break.
	self shouldnt: [ obj2 v2 ] raise: Break.
	self shouldnt: [ obj2 v2: 0 ] raise: Break.
	self shouldnt: [ obj4 v1 ] raise: Break.
	self shouldnt: [ obj4 v1: 0 ] raise: Break.
	self shouldnt: [ obj4 v2 ] raise: Break.
	self shouldnt: [ obj4 v2: 0 ] raise: Break.
]

{ #category : #'tests - object-centric api' }
VariableBreakpointTest >> testBreakOnWrite [
	wp := obj2 haltOnWrite.
	self shouldnt: [ obj2 v1 ] raise: Break.
	self should: [ obj2 v1: 0 ] raise: Break.
	self shouldnt: [ obj2 v2 ] raise: Break.
	self should: [ obj2 v2: 0 ] raise: Break.
	self shouldnt: [ obj4 v1 ] raise: Break.
	self shouldnt: [ obj4 v1: 0 ] raise: Break.
	self shouldnt: [ obj4 v2 ] raise: Break.
	self shouldnt: [ obj4 v2: 0 ] raise: Break.
]

{ #category : #'tests - object-centric api' }
VariableBreakpointTest >> testBreakOnWriteTo [

	wp := obj2 haltOnWriteTo: #v1.
	self shouldnt: [ obj2 v1 ] raise: Break.
	self should: [ obj2 v1: 0 ] raise: Break.
	self shouldnt: [ obj2 v2 ] raise: Break.
	self shouldnt: [ obj2 v2: 0 ] raise: Break.
	self shouldnt: [ obj4 v1 ] raise: Break.
	self shouldnt: [ obj4 v1: 0 ] raise: Break.
	self shouldnt: [ obj4 v2 ] raise: Break.
	self shouldnt: [ obj4 v2: 0 ] raise: Break.
]

{ #category : #'tests - temp var breakpoints' }
VariableBreakpointTest >> testBreakpointOnAllTempAccesses [
	|method|
	method := (VariableBreakpointMockClass>> #methodWithTempsAndArg:).
	wp := VariableBreakpoint watchVariable: #temp inClass: method.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #temp).
	self assert: wp targetClassOrMethod identicalTo: method.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: self allTempNodes asIdentitySet.	
	self should: [obj1 methodWithTempsAndArg: 42] raise: Break	
	
]

{ #category : #'tests - temp var breakpoints' }
VariableBreakpointTest >> testBreakpointOnArgument [
	|method|
	method := (VariableBreakpointMockClass>> #methodWithTempsAndArg:).
	wp := VariableBreakpoint watchVariable: #arg inClass: method.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #arg).
	self assert: wp targetClassOrMethod identicalTo: method.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: self argNodes asIdentitySet.	
	self should: [obj1 methodWithTempsAndArg: 42] raise: Break	
	
]

{ #category : #'tests - class var breakpoints' }
VariableBreakpointTest >> testBreakpointOnClassVarAccesses [
	|method|
	method := (VariableBreakpointMockClass>> #classVarRead).
	wp := VariableBreakpoint watchVariable: #ClassVar inClass: VariableBreakpointMockClass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #ClassVar).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockClass.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: (VariableBreakpointMockClass classVariableNamed: #ClassVar) accessingNodes asIdentitySet.
	self should: [obj1 classVarRead] raise: Break.
	self should: [obj1 classVarWrite] raise: Break	
]

{ #category : #'tests - class var breakpoints' }
VariableBreakpointTest >> testBreakpointOnClassVarReads [
	|method|
	method := (VariableBreakpointMockClass>> #classVarRead).
	wp := VariableBreakpoint watchVariableReads: #ClassVar inClass: VariableBreakpointMockClass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #ClassVar).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockClass.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: (VariableBreakpointMockClass classVariableNamed: #ClassVar) readNodes asIdentitySet.
	self should: [obj1 classVarRead] raise: Break	
]

{ #category : #'tests - class var breakpoints' }
VariableBreakpointTest >> testBreakpointOnClassVarWrites [
	|method|
	method := (VariableBreakpointMockClass>> #classVarWrite).
	wp := VariableBreakpoint watchVariableWrites: #ClassVar inClass: VariableBreakpointMockClass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #ClassVar).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockClass.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: (VariableBreakpointMockClass classVariableNamed: #ClassVar) assignmentNodes asIdentitySet.
	self should: [obj1 classVarWrite] raise: Break	
]

{ #category : #'tests - temp var breakpoints' }
VariableBreakpointTest >> testBreakpointOnTempReads [
	|method|
	method := (VariableBreakpointMockClass>> #methodWithTempsAndArg:).
	wp := VariableBreakpoint watchVariableReads: #temp inClass: method.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #temp).
	self assert: wp targetClassOrMethod identicalTo: method.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: self tempReadNodes asIdentitySet.	
	self should: [obj1 methodWithTempsAndArg: 42] raise: Break	
	
]

{ #category : #'tests - temp var breakpoints' }
VariableBreakpointTest >> testBreakpointOnTempWrites [
	|method|
	method := (VariableBreakpointMockClass>> #methodWithTempsAndArg:).
	wp := VariableBreakpoint watchVariableWrites: #temp inClass: method.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #temp).
	self assert: wp targetClassOrMethod identicalTo: method.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: self tempWriteNodes asIdentitySet.	
	self should: [obj1 methodWithTempsAndArg: 42] raise: Break	
	
]

{ #category : #'tests - initialization' }
VariableBreakpointTest >> testInitialize [ 	
	wp := VariableBreakpoint new.
	self assert: wp accessStrategy equals: #all.
	self assertEmpty: wp targetVariableNames.
	self deny: wp isInstalled
]

{ #category : #'tests - installation' }
VariableBreakpointTest >> testInstall [
	
	wp := VariableBreakpoint newForClass: VariableBreakpointMockSubclass.	
	wp install.
	self assertCollection: Breakpoint all includesAll: (OrderedCollection with: wp)
]

{ #category : #'tests - installation' }
VariableBreakpointTest >> testInstrumentedMethodIsCompiledMethod [


	wp := VariableBreakpointMockClass newBreakpointForVariable: #v1.
	wp install.
	self assert: (VariableBreakpointMockClass>>#v1) isCompiledMethod 
]

{ #category : #'tests - installation' }
VariableBreakpointTest >> testIsInstalled [
	
	wp := VariableBreakpoint newForClass: VariableBreakpointMockSubclass.
	self deny: wp isInstalled.
	wp install.
	self assert: wp isInstalled
]

{ #category : #'tests - initialization' }
VariableBreakpointTest >> testIsVariableBreakpoint [
	self assert: VariableBreakpoint new isVariableBreakpoint.
	self deny: Breakpoint new isVariableBreakpoint
]

{ #category : #'tests - initialization' }
VariableBreakpointTest >> testNewBreakpointForVariableInClass [
	wp := VariableBreakpoint newBreakpointForVariable: #var inClass: #class.
	self assert: wp class identicalTo: VariableBreakpoint.
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #var).
	self assert: wp targetClassOrMethod equals: #class
]

{ #category : #'tests - initialization' }
VariableBreakpointTest >> testNewBreakpointForVariableInClassForMethod [
]

{ #category : #'tests - initialization' }
VariableBreakpointTest >> testNewForClass [
	wp := VariableBreakpoint newForClass: VariableBreakpointMockSubclass.
	self assert: wp class identicalTo: VariableBreakpoint.
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #v1 with: #v2).
	self assert: wp targetClassOrMethod equals: VariableBreakpointMockSubclass
]

{ #category : #'tests - removing' }
VariableBreakpointTest >> testNoRemoveAfterSubclassRemoved [
	"Removing the class where the target variable of a variable breakpoint is defined should uninstall the variable breakpoint"
	|testClassNode testSubclassNode|
	self compileTestClass.
	self compileTestClass2.
	testClassNode := (testClass >> #accessingV1) ast allChildren detect:[:n| n isVariable].
	testSubclassNode := (testSubclass >> #accessingV1) ast allChildren detect:[:n| n isVariable].
	
	wp := VariableBreakpoint watchVariable: #v1 inClass: testClass. 
	wp install.	
	
	self should: [testClass basicNew accessingV1] raise: Break.
	
	testSubclass removeFromSystem.	

	self deny: (wp link nodes anySatisfy:[:n| n == testSubclassNode]).
	self assert: (wp link nodes anySatisfy:[:n| n == testClassNode]).
	self assert: wp isInstalled.
	self should: [ testClass basicNew accessingV1 ] raise: Break

]

{ #category : #'tests - notifications' }
VariableBreakpointTest >> testNotifyArgumentBreakpointHit [
	|method notification|
	method := (VariableBreakpointMockClass>> #methodWithTempsAndArg:).
	wp := VariableBreakpoint watchVariable: #arg inClass: method.
	self should: [obj1 methodWithTempsAndArg: 42] raise: Break.
	notification := observer tag.
	self assert: notification breakpoint identicalTo: wp.
	self assert: notification nodes size equals: 1.
	self assert: notification nodes first equals: method ast statements first value.
	self assert: notification valueOrNil equals: 42
]

{ #category : #'tests - notifications' }
VariableBreakpointTest >> testNotifyInstVarBreakpointHit [
	|method notification|
	obj1 v1: 0.
	method := (VariableBreakpointMockClass>> #v1).
	wp := VariableBreakpoint watchVariableReads: #v1 inClass: VariableBreakpointMockClass.
	self should: [obj1 v1] raise: Break.
	
	notification := observer tag.
	self assert: notification breakpoint identicalTo: wp.
	self assert: notification nodes size equals: 1.
	self assert: notification nodes first equals: method ast statements first value.
	self assert: notification valueOrNil equals: 0.
	
	wp remove.
	observer update: nil.
	method := (VariableBreakpointMockClass>> #v1:).
	wp := VariableBreakpoint watchVariableWrites: #v1 inClass: VariableBreakpointMockClass.	
	self should: [obj1 v1: 42] raise: Break.
	
	notification := observer tag.
	self assert: notification breakpoint identicalTo: wp.
	self assert: notification nodes size equals: 1.
	self assert: notification nodes first equals: method ast statements first.
	self assert: notification valueOrNil equals: 42.
]

{ #category : #'tests - notifications' }
VariableBreakpointTest >> testNotifyTempBreakpointHit [
	|method notification|
	method := (VariableBreakpointMockClass>> #methodWithTempsAndArg:).
	wp := VariableBreakpoint watchVariableReads: #temp inClass: method.
	self should: [obj1 methodWithTempsAndArg: 42] raise: Break.
	
	notification := observer tag.
	self assert: notification breakpoint identicalTo: wp.
	self assert: notification nodes size equals: 1.
	self assert: notification nodes first equals: method ast statements last value.
	self assert: notification valueOrNil equals: 42.
	
	wp remove.
	observer update: nil.
	wp := VariableBreakpoint watchVariableWrites: #temp inClass: method.
	self should: [obj1 methodWithTempsAndArg: 42] raise: Break.
	
	notification := observer tag.
	self assert: notification breakpoint identicalTo: wp.
	self assert: notification nodes size equals: 1.
	self assert: notification nodes first equals: method ast statements first.
	self assert: notification valueOrNil equals: 42
]

{ #category : #'tests - removing' }
VariableBreakpointTest >> testRemove [

	wp := VariableBreakpoint watchVariablesInClass: VariableBreakpointMockSubclass.
	wp remove.
	
	self denyCollection: Breakpoint all includesAll: (OrderedCollection with: wp).	
	self assertEmpty: wp link nodes.
	self assertEmpty: (self allNodes asIdentitySet flatCollect:[:n| n links]).
	
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [obj1 v1] raise: Break.	
	self shouldnt: [obj1 v1: 0] raise: Break.
	self shouldnt: [VariableBreakpointMockSubclass new ] raise: Break.
	self shouldnt: [obj2 instVarReads] raise: Break.
	self shouldnt: [obj2 instVarWrites] raise: Break.
	self shouldnt: [obj2 v2] raise: Break.	
	self shouldnt: [obj2 v2: 0] raise: Break.
	self shouldnt: [obj2 v1] raise: Break.	
	self shouldnt: [obj2 v1: 0] raise: Break.
	
]

{ #category : #'tests - removing' }
VariableBreakpointTest >> testRemoveAfterClassRemoved [
	"Removing the class where the target variable of a variable breakpoint is defined should uninstall the variable breakpoint"
	|testClassNodes|
	self compileTestClass.
	testClassNodes := (testClass >> #accessingV1) ast allChildren.
	
	wp := VariableBreakpoint watchVariable: #v1 inClass: testClass. 
	wp install.	
		
	testClass removeFromSystem.	
	self denyCollection: wp link nodes includesAny: testClassNodes.
	self deny: wp isInstalled.
	self shouldnt: [ VariableBreakpointMockClass new v1 ] raise: Break

]

{ #category : #'tests - removing' }
VariableBreakpointTest >> testRemoveAfterClassWithTempVarRemoved [
	"Removing the class where the target temporary variable of a variable breakpoint is defined should uninstall the variable breakpoint"
	|testClassNodes|
	self compileTestClass.
	testClassNodes := (testClass >> #accessingtemp) ast allChildren.
	
	wp := (testClass >> #accessingtemp)  newBreakpointForVariable: #temp.
	wp install.	
	self should: [ testClass basicNew accessingtemp ] raise: Break.
		
	testClass removeFromSystem.	
	self denyCollection: wp link nodes includesAny: testClassNodes.
	self deny: wp isInstalled.
	self shouldnt: [ testClass basicNew accessingtemp ] raise: Break

]

{ #category : #'tests - removing' }
VariableBreakpointTest >> testRemoveAfterSuperclassRemoved [
	"Removing the class where the target variable of a variable breakpoint is defined should uninstall the variable breakpoint"
	|testClassNodes|
	self compileTestClass.
	self compileTestClass2.
	testClassNodes := (testClass >> #accessingV1) ast allChildren.
	
	wp := VariableBreakpoint watchVariable: #v1 inClass: testClass. 
	wp install.	
	
	self should: [testSubclass basicNew accessingV1] raise: Break.
	
	testClass removeFromSystem.	
	self denyCollection: wp link nodes includesAny: testClassNodes.
	self deny: wp isInstalled.
	self shouldnt: [ testSubclass basicNew accessingV1 ] raise: Break

]

{ #category : #'tests - removing' }
VariableBreakpointTest >> testRemoveFromMethod [
	
	|ast|
	"We get the variable node one which the variable breakpoint will be installed"
	ast := (VariableBreakpointMockClass >> #v1) ast allChildren last. 
	
	"Removing a variable breakpoint should not uninstall the breakpoint"
	wp := VariableBreakpoint watchVariable: #v1 inClass: VariableBreakpointMockSubclass. 
	wp install.		
		
	"Let us recompile a method touching v1 in the super class of our test class.
	This is meant so simulate, e.g., a modification in the debugger after a break in that method."
	VariableBreakpointMockClass compile: 'v1 ^v1'.
	
	"First, the breakpoint should still be installed in that method"
	self assert: wp isInstalled.
	self should: [ VariableBreakpointMockClass basicNew v1 ] raise: Break.
	self should: [ VariableBreakpointMockSubclass basicNew v1 ] raise: Break.
	
	"Second, the old method ast's is not referenced by the breakpoint's link anymore"
	self assert: (wp link nodes noneSatisfy: [ :n| n == ast ])
	
]

{ #category : #'tests - installation' }
VariableBreakpointTest >> testScopeTo [
	| instance testContext |
	instance := Object new.
	testContext := Context newForMethod: VariableBreakpointMockClass >> #v1.
	wp := VariableBreakpoint new.
	wp scopeTo: instance.
	self assert: wp targetInstance identicalTo: instance.
	self assert: wp link hasCondition.
	self assertCollection: wp link conditionArguments equals: #(context).
	self
		assert: (wp link condition value: (testContext receiver: instance)).
	self
		deny: (wp link condition value: (testContext receiver: Object new))
]

{ #category : #'tests - installation' }
VariableBreakpointTest >> testSetAsBreakpointProperty [
	 | nodes |
	 wp := VariableBreakpoint
		       watchVariable: #v1
		       inClass: VariableBreakpointMockClass.
	 nodes := wp link nodes.
	 nodes do: [ :n | 
		 self assert: n hasBreakpoint.
		 self assertCollection: n breakpoints equals: { wp } asSet ].
	 wp remove.
	 nodes do: [ :n | self deny: n hasBreakpoint ]
]

{ #category : #'tests - class wide watchpoints' }
VariableBreakpointTest >> testWatchVariableInClass [ 	

	wp := VariableBreakpoint watchVariable: #v1 inClass: VariableBreakpointMockClass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #v1).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockClass.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet equals: self allNodesForV1 asIdentitySet.
	
	self should: [VariableBreakpointMockClass new] raise: Break.
	self should: [obj1 v1] raise: Break.	
	self should: [obj1 v1: 0] raise: Break.
	self should: [ VariableBreakpointMockSubclass new ] raise: Break.
	self should: [ obj2 instVarReads] raise: Break.
	self should: [ obj2 instVarWrites] raise: Break
	
	
]

{ #category : #'tests - object-centric' }
VariableBreakpointTest >> testWatchVariableInObject [
	wp := VariableBreakpoint watchVariable: #v1 inObject: obj1.
	self assert: wp targetInstance equals: obj1.	
	
	self should: [obj1 v1] raise: Break.	
	self should: [obj1 v1: 0] raise: Break.	
		
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [obj3 v1] raise: Break.
	self shouldnt: [obj3 v1: 0] raise: Break
	
	
]

{ #category : #'tests - class wide watchpoints' }
VariableBreakpointTest >> testWatchVariableReadsInClass [ 	

	wp := VariableBreakpoint watchVariableReads: #v1 inClass: VariableBreakpointMockClass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #v1).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockClass.
	self assert: wp targetInstance equals: nil.

	self assertCollection: wp link nodes asIdentitySet 
			equals: (self allNodesForV1 select:[:n| n isVariable]) asIdentitySet.
	
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self should: [obj1 v1] raise: Break.	
	self shouldnt: [obj1 v1: 0] raise: Break.
	self shouldnt: [ VariableBreakpointMockSubclass new ] raise: Break.
	self should: [ obj2 instVarReads] raise: Break.
	self shouldnt: [ obj2 instVarWrites] raise: Break
	
	
]

{ #category : #'tests - object-centric' }
VariableBreakpointTest >> testWatchVariableReadsInObject [ 	

	wp := VariableBreakpoint watchVariableReads: #v1 inObject: obj1.
	self assert: wp targetInstance equals: obj1.
	self assert: wp accessStrategy equals: #read.
	
	self should: [obj1 v1] raise: Break.	
	self shouldnt: [obj1 v1: 0] raise: Break.
	
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [obj3 v1] raise: Break.
	self shouldnt: [obj3 v1: 0] raise: Break
	
	
]

{ #category : #'tests - class wide watchpoints' }
VariableBreakpointTest >> testWatchVariableWritesInClass [ 	

	wp := VariableBreakpoint watchVariableWrites: #v1 inClass: VariableBreakpointMockClass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #v1).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockClass.
	self assert: wp targetInstance equals: nil.

	self assertCollection: wp link nodes asIdentitySet 
			equals: (self allNodesForV1 select:[:n| n isAssignment]) asIdentitySet.
	
	self should: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [obj1 v1] raise: Break.	
	self should: [obj1 v1: 0] raise: Break.
	self should: [ VariableBreakpointMockSubclass new ] raise: Break.
	self shouldnt: [ obj2 instVarReads] raise: Break.
	self should: [ obj2 instVarWrites] raise: Break
	
	
	
	
]

{ #category : #'tests - object-centric' }
VariableBreakpointTest >> testWatchVariableWritesInObject [ 	

	wp := VariableBreakpoint watchVariableWrites: #v1 inObject: obj1.	
	self assert: wp targetInstance equals: obj1.
	self assert: wp accessStrategy equals: #write.
		
	self should: [obj1 v1: 0] raise: Break.
	self shouldnt: [obj1 v1] raise: Break.		
			
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [obj3 v1] raise: Break.
	self shouldnt: [obj3 v1: 0] raise: Break
	
	
	
	
]

{ #category : #'tests - class wide watchpoints' }
VariableBreakpointTest >> testWatchVariablesInClass [	

	wp := VariableBreakpoint watchVariablesInClass: VariableBreakpointMockSubclass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #v1 with: #v2).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockSubclass.
	self assert: wp targetInstance equals: nil.

	self assertCollection: wp link nodes asIdentitySet equals: self allNodes asIdentitySet.
	
	self should: [VariableBreakpointMockClass new] raise: Break.
	self should: [obj1 v1] raise: Break.	
	self should: [obj1 v1: 0] raise: Break.
	self should: [VariableBreakpointMockSubclass new ] raise: Break.
	self should: [obj2 instVarReads] raise: Break.
	self should: [obj2 instVarWrites] raise: Break.
	self should: [obj2 v2] raise: Break.	
	self should: [obj2 v2: 0] raise: Break.
	self should: [obj2 v1] raise: Break.	
	self should: [obj2 v1: 0] raise: Break.
	
]

{ #category : #'tests - object-centric' }
VariableBreakpointTest >> testWatchVariablesInObject [	

	wp := VariableBreakpoint watchVariablesInObject: obj2.	
	self assert: wp targetInstance equals: obj2.	
	
	self should: [obj2 instVarReads] raise: Break.
	self should: [obj2 instVarWrites] raise: Break.
	self should: [obj2 v2] raise: Break.	
	self should: [obj2 v2: 0] raise: Break.
	self should: [obj2 v1] raise: Break.	
	self should: [obj2 v1: 0] raise: Break.
	
	self shouldnt: [obj4 instVarReads] raise: Break.
	self shouldnt: [obj4 instVarWrites] raise: Break.
	self shouldnt: [obj4 v2] raise: Break.	
	self shouldnt: [obj4 v2: 0] raise: Break.
	self shouldnt: [obj4 v1] raise: Break.	
	self shouldnt: [obj4 v1: 0] raise: Break.
	
	self shouldnt: [obj1 v1] raise: Break.	
	self shouldnt: [obj1 v1: 0] raise: Break.
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [VariableBreakpointMockSubclass new ] raise: Break.
	
]

{ #category : #'tests - class wide watchpoints' }
VariableBreakpointTest >> testWatchVariablesReadsInClass [	

	wp := VariableBreakpoint watchVariablesReadsInClass: VariableBreakpointMockSubclass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #v1 with: #v2).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockSubclass.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet 
			equals: (self allNodes select:[:n| n isVariable]) asIdentitySet.
	
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self should: [obj1 v1] raise: Break.	
	self shouldnt: [obj1 v1: 0] raise: Break.
	self shouldnt: [VariableBreakpointMockSubclass new ] raise: Break.
	self should: [obj2 instVarReads] raise: Break.
	self shouldnt: [obj2 instVarWrites] raise: Break.
	self should: [obj2 v2] raise: Break.	
	self shouldnt: [obj2 v2: 0] raise: Break.
	self should: [obj2 v1] raise: Break.	
	self shouldnt: [obj2 v1: 0] raise: Break.
	
]

{ #category : #'tests - object-centric' }
VariableBreakpointTest >> testWatchVariablesReadsInObject [	

	wp := VariableBreakpoint watchVariablesReadsInObject: obj2.
	self assert: wp targetInstance equals: obj2.
	self assert: wp accessStrategy equals: #read.	 
	
	self should: [obj2 v2] raise: Break.		
	self should: [obj2 instVarReads] raise: Break.
	self should: [obj2 v1] raise: Break.
	self shouldnt: [obj2 instVarWrites] raise: Break.
	self shouldnt: [obj2 v1: 0] raise: Break.
	self shouldnt: [obj2 v2: 0] raise: Break.
	
	
	self shouldnt: [obj4 instVarReads] raise: Break.
	self shouldnt: [obj4 instVarWrites] raise: Break.
	self shouldnt: [obj4 v2] raise: Break.	
	self shouldnt: [obj4 v2: 0] raise: Break.
	self shouldnt: [obj4 v1] raise: Break.	
	self shouldnt: [obj4 v1: 0] raise: Break.
	
	self shouldnt: [obj1 v1] raise: Break.	
	self shouldnt: [obj1 v1: 0] raise: Break.
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [VariableBreakpointMockSubclass new ] raise: Break.
	
]

{ #category : #'tests - class wide watchpoints' }
VariableBreakpointTest >> testWatchVariablesWritesInClass [	

	wp := VariableBreakpoint watchVariablesWritesInClass: VariableBreakpointMockSubclass.
	
	self assertCollection: wp targetVariableNames equals: (OrderedCollection with: #v1 with: #v2).
	self assert: wp targetClassOrMethod identicalTo: VariableBreakpointMockSubclass.
	self assert: wp targetInstance equals: nil.
	
	self assertCollection: wp link nodes asIdentitySet 
			equals: (self allNodes select:[:n| n isAssignment]) asIdentitySet.
	
	self should: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [obj1 v1] raise: Break.	
	self should: [obj1 v1: 0] raise: Break.
	self should: [VariableBreakpointMockSubclass new ] raise: Break.
	self shouldnt: [obj2 instVarReads] raise: Break.
	self should: [obj2 instVarWrites] raise: Break.
	self shouldnt: [obj2 v2] raise: Break.	
	self should: [obj2 v2: 0] raise: Break.
	self shouldnt: [obj2 v1] raise: Break.	
	self should: [obj2 v1: 0] raise: Break.
	
]

{ #category : #'tests - object-centric' }
VariableBreakpointTest >> testWatchVariablesWritesInObject [	

	wp := VariableBreakpoint watchVariablesWritesInObject: obj2.
	self assert: wp targetInstance equals: obj2.
	self assert: wp accessStrategy equals: #write.	
	
	self should: [obj2 instVarWrites] raise: Break.
	self should: [obj2 v1: 0] raise: Break.
	self should: [obj2 v2: 0] raise: Break.
	self shouldnt: [obj2 v2] raise: Break.		
	self shouldnt: [obj2 instVarReads] raise: Break.
	self shouldnt: [obj2 v1] raise: Break.		
	
	self shouldnt: [obj4 instVarReads] raise: Break.
	self shouldnt: [obj4 instVarWrites] raise: Break.
	self shouldnt: [obj4 v2] raise: Break.	
	self shouldnt: [obj4 v2: 0] raise: Break.
	self shouldnt: [obj4 v1] raise: Break.	
	self shouldnt: [obj4 v1: 0] raise: Break.
	
	self shouldnt: [obj1 v1] raise: Break.	
	self shouldnt: [obj1 v1: 0] raise: Break.
	self shouldnt: [VariableBreakpointMockClass new] raise: Break.
	self shouldnt: [VariableBreakpointMockSubclass new ] raise: Break.
	
]
