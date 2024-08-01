Class {
	#name : 'DebugPointObserverTest',
	#superclass : 'TestCase',
	#instVars : [
		'dp',
		'node',
		'observer'
	],
	#category : 'DebugPoints-Tests-Model',
	#package : 'DebugPoints-Tests',
	#tag : 'Model'
}

{ #category : 'running' }
DebugPointObserverTest >> setUp [

	super setUp.

	observer := DummyDebugPointObserver new.
	"set a node to install the dp on"
	node := (DummyTestClass methods select: [ :method |
		         method name = 'DummyTestClass>>#id:' ]) first ast
]

{ #category : 'running' }
DebugPointObserverTest >> tearDown [

	dp ifNotNil: [ dp remove ].
	self class codeSupportAnnouncer unsubscribe: observer.

	super tearDown
]

{ #category : 'tests' }
DebugPointObserverTest >> testNotifyDebugPointAdded [

	self class codeSupportAnnouncer when: DebugPointAdded send: #update: to: observer.
	dp := DebugPointManager installNew: DebugPoint on: node.
	self assert: observer tag class equals: DebugPointAdded.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id:) ast } asSet
]

{ #category : 'tests' }
DebugPointObserverTest >> testNotifyDebugPointChanged [

	dp := DebugPointManager installNew: DebugPoint on: node.
	self class codeSupportAnnouncer when: DebugPointChanged send: #update: to: observer.

	dp addBehavior: OnceBehavior new.

	self assert: observer tag class equals: DebugPointChanged.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id:) ast } asSet.

	observer tag: nil.
	dp removeBehavior: OnceBehavior.

	self assert: observer tag class equals: DebugPointChanged.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id:) ast } asSet
]

{ #category : 'tests' }
DebugPointObserverTest >> testNotifyDebugPointHit [

	self class codeSupportAnnouncer when: DebugPointHit send: #update: to: observer.
	dp := DebugPointManager installNew: DebugPoint on: node.

	DummyTestClass new id: 2.

	self assert: observer tag class equals: DebugPointHit.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id:) ast } asSet
]

{ #category : 'tests' }
DebugPointObserverTest >> testNotifyDebugPointRemoved [

	dp := DebugPointManager installNew: DebugPoint on: node.
	self class codeSupportAnnouncer when: DebugPointRemoved send: #update: to: observer.
	dp remove.
	self assert: observer tag class equals: DebugPointRemoved.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id:) ast } asSet
]

{ #category : 'tests' }
DebugPointObserverTest >> testNotifyVariableDebugPointHit [

	| obj |
	self class codeSupportAnnouncer when: DebugPointHit send: #update: to: observer.
	dp := DebugPointManager installNew: DebugPoint inClass: DummyTestClass onVariableAccessNamed: #id.

	obj := DummyTestClass new.

	self assert: observer tag class equals: VariableDebugPointHit.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #initialize) ast statements second } asSet.
	self assert: observer tag variableValue equals: nil.

	observer tag: nil.
	obj id.

	self assert: observer tag class equals: VariableDebugPointHit.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id) ast statements first value } asSet.
	self assert: observer tag variableValue equals: 0.

	observer tag: nil.
	obj id: 2.

	self assert: observer tag class equals: VariableDebugPointHit.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id:) ast statements first value } asSet.
	self assert: observer tag variableValue equals: 0.

	observer tag: nil.
	obj id.

	self assert: observer tag class equals: VariableDebugPointHit.
	self assert: observer tag debugPoint identicalTo: dp.
	self assertCollection: observer tag nodes equals: { (DummyTestClass >> #id) ast statements first value } asSet.
	self assert: observer tag variableValue equals: 2
]
