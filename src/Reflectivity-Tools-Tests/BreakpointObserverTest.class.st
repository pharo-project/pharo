Class {
	#name : #BreakpointObserverTest,
	#superclass : #TestCase,
	#instVars : [
		'breakpoint',
		'cls',
		'previousBreakpoints',
		'observer',
		'oldSystemAnnouncer'
	],
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #helpers }
BreakpointObserverTest >> newDummyClass [

	^ self class classInstaller make: [ :aBuilder | 
		  aBuilder
			  name: #DummyClassForBreakpoint;
			  package: 'DummyPackage' ]
]

{ #category : #running }
BreakpointObserverTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	super setUp.
	previousBreakpoints := Breakpoint all copy.
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.
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
	cls ifNotNil: [ cls isObsolete ifFalse: [ cls removeFromSystem ] ].
	pkg := 'DummyPackage' asPackageIfAbsent: [ ]. 
	pkg ifNotNil: [ pkg removeFromSystem ].
	Breakpoint removeAll.
	Breakpoint all addAll: previousBreakpoints.
	SystemAnnouncer announcer: oldSystemAnnouncer.
	super tearDown
]

{ #category : #tests }
BreakpointObserverTest >> testNotifyBreakpointAdded [
	SystemAnnouncer uniqueInstance when: BreakpointAdded send: #update: to: observer.
	breakpoint install.
	self assert: observer tag class equals: BreakpointAdded.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: { (cls >> #dummy) ast } asSet
]

{ #category : #tests }
BreakpointObserverTest >> testNotifyBreakpointHit [
	SystemAnnouncer uniqueInstance when: BreakpointHit send: #update: to: observer.
	breakpoint install.
	self should: [cls new dummy] raise: Break.	
	self assert: observer tag class equals: BreakpointHit.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assert: observer tag valueOrNil isNil
	
]

{ #category : #tests }
BreakpointObserverTest >> testNotifyBreakpointRemoved [	
	breakpoint install.
	SystemAnnouncer uniqueInstance when: BreakpointRemoved send: #update: to: observer.
	breakpoint remove.
	self assert: observer tag class equals: BreakpointRemoved.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: { (cls >> #dummy) ast } asSet
]
