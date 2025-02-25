Class {
	#name : 'BreakpointObserverTest',
	#superclass : 'TestCase',
	#instVars : [
		'breakpoint',
		'cls',
		'previousBreakpoints',
		'observer',
		'oldSystemAnnouncer'
	],
	#category : 'Reflectivity-Tools-Tests',
	#package : 'Reflectivity-Tools-Tests'
}

{ #category : 'helpers' }
BreakpointObserverTest >> newDummyClass [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: #DummyClassForBreakpoint;
			  package: 'DummyPackage' ]
]

{ #category : 'running' }
BreakpointObserverTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	super setUp.
	previousBreakpoints := Breakpoint all copy.
	oldSystemAnnouncer := self class codeSupportAnnouncer.
	self class environment codeSupportAnnouncer: nil.
	cls := self newDummyClass.
	breakpoint := Breakpoint new.
	cls compile: 'dummy ^42'.
	breakpoint
		node: (cls >> #dummy) ast;
		once: true.
	observer := DummyBreakpointObserver new
]

{ #category : 'running' }
BreakpointObserverTest >> tearDown [

	cls ifNotNil: [ cls isObsolete ifFalse: [ cls removeFromSystem ] ].
	self packageOrganizer removePackage: 'DummyPackage'.
	Breakpoint removeAll.
	Breakpoint all addAll: previousBreakpoints.
	self class environment codeSupportAnnouncer: oldSystemAnnouncer.
	super tearDown
]

{ #category : 'tests' }
BreakpointObserverTest >> testNotifyBreakpointAdded [
	self class codeSupportAnnouncer when: BreakpointAdded send: #update: to: observer.
	breakpoint install.
	self assert: observer tag class equals: BreakpointAdded.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: { (cls >> #dummy) ast } asSet
]

{ #category : 'tests' }
BreakpointObserverTest >> testNotifyBreakpointHit [
	self class codeSupportAnnouncer when: BreakpointHit send: #update: to: observer.
	breakpoint install.
	self should: [cls new dummy] raise: Break.
	self assert: observer tag class equals: BreakpointHit.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assert: observer tag valueOrNil isNil
]

{ #category : 'tests' }
BreakpointObserverTest >> testNotifyBreakpointRemoved [
	breakpoint install.
	self class codeSupportAnnouncer when: BreakpointRemoved send: #update: to: observer.
	breakpoint remove.
	self assert: observer tag class equals: BreakpointRemoved.
	self assert: observer tag breakpoint identicalTo: breakpoint.
	self assertCollection: observer tag nodes equals: { (cls >> #dummy) ast } asSet
]
