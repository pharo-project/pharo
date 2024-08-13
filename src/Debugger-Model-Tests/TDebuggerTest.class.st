Class {
	#name : 'TDebuggerTest',
	#superclass : 'TestCase',
	#traits : 'TDebugger',
	#classTraits : 'TDebugger classTrait',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'accessing' }
TDebuggerTest class >> closeAllDebuggers [
	"Do nothing. This class is a test class, not a real debugger, so it cannot stay open like real debugger. Therefore, it has nothing to do to 'close all of its debuggers'"

	^ self
]

{ #category : 'instance creation' }
TDebuggerTest class >> debugSession: aDebugSession [
	^true
]

{ #category : 'debugging' }
TDebuggerTest class >> handlesDebugSession: aDebugSession [

	^ true
]

{ #category : 'tests' }
TDebuggerTest >> testAvailableAutomatically [
	self class availableAutomatically: nil.
	self
		assert: self class availableAutomatically
		equals: DebuggerSettings defaultAvailability.
	self class availableAutomatically: true.
	self assert: self class availableAutomatically.
	self class availableAutomatically: false.
	self deny: self class availableAutomatically
]

{ #category : 'tests' }
TDebuggerTest >> testDebugSession [
	self assert: (self class debugSession: nil)
]

{ #category : 'tests' }
TDebuggerTest >> testHandlesContext [
	"By default handles any context"

	self assert: (self class handlesDebugSession:
			 ((DebugSession
				   named: 'a random debug session'
				   on: nil
				   startedAt: nil)
				  exception: Exception new;
				  yourself))
]

{ #category : 'tests' }
TDebuggerTest >> testRank [
	self class rank: nil.
	self assert: self class rank equals: DebuggerSettings defaultDebuggerRank.
	self class rank: 1.
	self assert: self class rank equals: 1
]

{ #category : 'tests' }
TDebuggerTest >> testSystemDebuggers [

	| debuggers traitUsersWithSubclasses |
	debuggers := DebuggerSettings systemDebuggers.
	traitUsersWithSubclasses := TDebugger users flatCollect: [ :class |
		                            class withAllSubclasses ].
	self assert: (debuggers allSatisfy: [ :dbg |
			 traitUsersWithSubclasses includes: dbg ]).
	self deny: (debuggers anySatisfy: [ :dbg | dbg isTestCase ])
]
