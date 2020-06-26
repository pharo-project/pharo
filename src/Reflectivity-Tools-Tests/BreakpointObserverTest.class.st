Class {
	#name : #BreakpointObserverTest,
	#superclass : #TestCase,
	#instVars : [
		'oldObservers',
		'breakpoint',
		'cls',
		'previousBreakpoints',
		'observer'
	],
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #helpers }
BreakpointObserverTest >> newDummyClass [
	^ Object
		subclass: #DummyClassForBreakpoint
		instanceVariableNames: ''
		classVariableNames: ''
		package: 'DummyPackage'
]

{ #category : #running }
BreakpointObserverTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	super setUp.
	previousBreakpoints := Breakpoint all copy.
	oldObservers := Breakpoint observers copy.
	Breakpoint observers removeAll.
	cls := self newDummyClass.
	breakpoint := Breakpoint new.
	cls compile: 'dummy ^42'.
	breakpoint
		node: (cls >> #dummy) ast;
		once.
	observer := DummyBreakpointObserver new
]

{ #category : #running }
BreakpointObserverTest >> tearDown [
	|pkg|
	Breakpoint observers removeAll.
	Breakpoint observers addAll: oldObservers.
	cls ifNotNil: [ cls isObsolete ifFalse: [ cls removeFromSystem ] ].
	pkg := 'DummyPackage' asPackageIfAbsent: [ ]. 
	pkg ifNotNil: [ pkg removeFromSystem ].
	Breakpoint removeAll.
	Breakpoint all addAll: previousBreakpoints.
	super tearDown
]

{ #category : #tests }
BreakpointObserverTest >> testNotifyBreakpointAdded [
	Breakpoint registerObserver: observer.
	breakpoint install.
	self assert: observer tag class equals: BreakpointAddedNotification.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: { (cls >> #dummy) ast } asSet
]

{ #category : #tests }
BreakpointObserverTest >> testNotifyBreakpointHit [
	Breakpoint registerObserver: observer.
	breakpoint install.
	self should: [cls new dummy] raise: Break.	
	self assert: observer tag class equals: BreakpointHitNotification.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assert: observer tag valueOrNil isNil
	
]

{ #category : #tests }
BreakpointObserverTest >> testNotifyBreakpointRemoved [	
	breakpoint install.
	Breakpoint registerObserver: observer.
	breakpoint remove.
	self assert: observer tag class equals: BreakpointRemovedNotification.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: { (cls >> #dummy) ast } asSet
]

{ #category : #tests }
BreakpointObserverTest >> testRegisterObserver [
	|obs|
	obs := DummyBreakpointObserver new.
	Breakpoint registerObserver: obs.
	self assertCollection: Breakpoint observers includesAll: {obs}
]

{ #category : #tests }
BreakpointObserverTest >> testUnregisterObserver [
	|obs|
	obs := DummyBreakpointObserver new.
	Breakpoint registerObserver: obs.
	Breakpoint unregisterObserver: obs.
	self assertEmpty: Breakpoint observers
]
