Class {
	#name : #TDebuggerTest,
	#superclass : #TestCase,
	#traits : 'TDebugger',
	#classTraits : 'TDebugger classTrait',
	#category : #'Debugger-Model-Tests-Core'
}

{ #category : #'instance creation' }
TDebuggerTest class >> debugSession: aDebugSession [
	^true
]

{ #category : #tests }
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

{ #category : #tests }
TDebuggerTest >> testDebugSession [
	self assert: (self class debugSession: nil)
]

{ #category : #tests }
TDebuggerTest >> testHandlesContext [
	"By default handles any context"
	self assert: (self class handlesContext: nil)
]

{ #category : #tests }
TDebuggerTest >> testRank [
	self class rank: nil.
	self assert: self class rank equals: DebuggerSettings defaultDebuggerRank.
	self class rank: 1.
	self assert: self class rank equals: 1
]

{ #category : #tests }
TDebuggerTest >> testSystemDebuggers [
	|debuggers|
	debuggers := DebuggerSettings systemDebuggers.
	self assert: (debuggers allSatisfy: [:dbg| TDebugger users includes: dbg]).
	self deny: (debuggers anySatisfy: [:dbg| dbg isTestCase]).
]
