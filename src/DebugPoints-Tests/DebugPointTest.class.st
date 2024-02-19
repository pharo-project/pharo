"
I am a test class that tests the behavior of different debug points on different targets with different behaviors
"
Class {
	#name : 'DebugPointTest',
	#superclass : 'TestCase',
	#instVars : [
		'dp',
		'node',
		'context',
		'dp2'
	],
	#category : 'DebugPoints-Tests-Model',
	#package : 'DebugPoints-Tests',
	#tag : 'Model'
}

{ #category : 'running' }
DebugPointTest >> setUp [
	super setUp.
	
	"set a node to install the dp on"
	node := (DummyTestClass methods select: [ :method | method name = 'DummyTestClass>>#id:' ]) first ast.
	context := self setUpContext.
	
	
]

{ #category : 'initialization' }
DebugPointTest >> setUpContext [
	"set up context"
	| aCompiledMethod aReceiver aSender anArgument aMethodContext |
	aCompiledMethod := Rectangle methodDict at: #areasOutside:.
	aReceiver := 100 @ 100 corner: 200 @ 200.
	aSender := thisContext.
	anArgument := 420 @ 420 corner: 200 @ 200.
	^aMethodContext := Context
		                  sender: aSender
		                  receiver: aReceiver
		                  method: aCompiledMethod
		                  arguments: { anArgument }.
]

{ #category : 'running' }
DebugPointTest >> tearDown [

	dp ifNotNil: [ dp remove ].
	dp2 ifNotNil: [ dp2 remove ].
	super tearDown
]

{ #category : 'tests' }
DebugPointTest >> testBehaviorPriority [
	| a b c set |
	dp := DebugPointManager installNew: DebugPoint on: node.
	a := OnceBehavior new priority: 2.
	b := CountBehavior new priority: 1.
	c := ChainBehavior new priority: 3.
	
	dp addBehavior: a.
	dp addBehavior: b.
	dp addBehavior: c.
	
	set := OrderedCollection new add: c; add: a; add: b; yourself.
	
	self assertCollection: (dp behaviors asOrderedCollection ) equals: set.

]

{ #category : 'tests' }
DebugPointTest >> testBreakDebugPoint [
	dp := DebugPointManager installNew: BreakDebugPoint on: node.
	self should: [dp hitWithContext: context] raise: Break.
	self should: [DummyTestClass new id:2 ] raise: Break.
]

{ #category : 'tests' }
DebugPointTest >> testBreakDebugPointOnVariableAccess [

	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      inClass: DummyTestClass
		      onVariableAccessNamed: #id.
	self should: [ DummyTestClass new ] raise: Break.
	self should: [ DummyTestClass basicNew id ] raise: Break.
	self shouldnt: [ DummyTestClass basicNew ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testBreakDebugPointOnVariableDetectsNewVariableAccessAfterCompilingNewMethod [

	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      inClass: DummyTestClass
		      onVariableWriteNamed: #id.
	self deny: (DummyTestClass includesSelector: #id2:).

	[
	DummyTestClass compiler
		source: 'id2: aNumber
		id := aNumber';
		install.
	self should: [ DummyTestClass basicNew id2: 42 ] raise: Break ]
		ensure: [ DummyTestClass removeSelector: #id2: ]
]

{ #category : 'tests' }
DebugPointTest >> testBreakDebugPointOnVariableRead [

	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      inClass: DummyTestClass
		      onVariableReadNamed: #id.
	self should: [ DummyTestClass basicNew id ] raise: Break.
	self shouldnt: [ DummyTestClass new ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testBreakDebugPointOnVariableWrite [

	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      inClass: DummyTestClass
		      onVariableWriteNamed: #id.
	self should: [ DummyTestClass new ] raise: Break.
	self shouldnt: [ DummyTestClass basicNew id ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testChainBehaviorHitWithContext [

	dp := DebugPointManager
		      installNew: DebugPoint
		      on: node
		      withBehaviors: { ChainBehavior }.
	dp2 := DebugPointManager
		       installNew: DebugPoint
		       on: node
		       withBehaviors: { ChainBehavior }.


	dp putChild: dp2.

	"initial states check"
	self assert: dp enabled equals: true.
	self assert: dp2 enabled equals: false.

	"hit parent, check states"
	dp hitWithContext: context.
	self assert: dp enabled equals: false.
	self assert: dp2 enabled equals: true.

	"hit child, check states"
	dp2 hitWithContext: context.
	self assert: dp enabled equals: false.
	self assert: dp2 enabled equals: false
]

{ #category : 'tests' }
DebugPointTest >> testChainBehaviorPutChild [
	dp := DebugPointManager installNew: DebugPoint  on: node withBehaviors: {ChainBehavior}.
	dp2 := DebugPointManager installNew: DebugPoint  on: node withBehaviors: {ChainBehavior}.
	
	"check parent/child"
	self assert: (dp child) equals: nil.
	self assert: (dp parent) equals: nil.
	
	dp putChild: dp2.
	
	self assert: (dp child) equals: dp2.
	self assert: (dp parent) equals: nil.
	self assert: (dp2 parent) equals: dp.
	
	
]

{ #category : 'tests' }
DebugPointTest >> testChainBehaviorRemoveBehavior [

	dp := DebugPointManager
		      installNew: DebugPoint
		      on: node
		      withBehaviors: { ChainBehavior }.
	dp2 := DebugPointManager
		       installNew: DebugPoint
		       on: node
		       withBehaviors: { ChainBehavior }.

	"check parent/child"
	dp putChild: dp2.
	"hit parent, check states"
	dp hitWithContext: context.

	self assert: dp child equals: dp2.
	self assert: dp parent equals: nil.
	self assert: dp2 parent equals: dp.

	self assert: dp enabled equals: false.
	self assert: dp2 enabled equals: true.

	"check parent/child after removal"
	dp removeBehavior: ChainBehavior.
	
	self assert: dp2 parent equals: nil.
	self assert: dp2 child equals: nil.
	
	self deny: dp enabled.
	self assert: dp2 enabled.

]

{ #category : 'tests' }
DebugPointTest >> testChainBehaviorResetChain [
	dp := DebugPointManager installNew: DebugPoint  on: node withBehaviors: {ChainBehavior}.
	dp2 := DebugPointManager installNew: DebugPoint  on: node withBehaviors: {ChainBehavior}.
	
	dp putChild: dp2.
	"hit parent, check states"
	dp hitWithContext: context.
	self assert: (dp enabled) equals: false.
	self assert: (dp2 enabled) equals: true.
	
	"reset states check"
	dp resetChain.

	self assert: (dp enabled) equals: true.
	self assert: (dp2 enabled) equals: false
	


]

{ #category : 'tests' }
DebugPointTest >> testConditionBehavior [
	| behavior |
	behavior := ConditionBehavior new condition: 'corner=(300@200)'.
	dp := DebugPointManager installNew: BreakDebugPoint on: node.
	dp addBehavior: behavior.
	"dp addBehavior: BreakBehavior new."

	"hit the point so the arguments are actually saved in the debug point"
	dp hitWithContext: context.
	"condition is false and should not trigger break"
	self assert: (behavior execute) equals: false.
	self shouldnt: [dp hitWithContext: context.] raise: Break.
	"condition is false and should not trigger break"
	behavior condition: 'self corner=(300@200)'.
	self assert: (behavior execute) equals: false.
	self shouldnt: [dp hitWithContext: context.] raise: Break.
	"condition is true and should trigger the break"
	behavior condition: 'corner=(200@200)'.
	self assert: (behavior execute) equals: true .
	self should: [dp hitWithContext: context.] raise: Break.

]

{ #category : 'tests' }
DebugPointTest >> testConditionBehaviorThisContext [
	| behavior |
	
	dp := DebugPointManager installNew: BreakDebugPoint on: node.

	behavior := ConditionBehavior new condition: 'thisContext method = 2'.
	dp addBehavior: behavior.
	
	"hit the point so the arguments are actually saved in the debug point"
	dp hitWithContext: context.
	
	"it should not break with incorrect condition"
	self assert: (behavior execute) equals: false.
	self shouldnt: [dp hitWithContext: context] raise: Break.

	"here the condition is correct and it should break"
	behavior condition: 'thisContext method = (Rectangle methodDict at: #areasOutside:)'.
	self assert: (behavior execute) equals: true .
	self should: [dp hitWithContext: context] raise: Break.

]

{ #category : 'tests' }
DebugPointTest >> testCountBehavior [
	| cbh |
	
	dp := DebugPointManager installNew: DebugPoint  on: node withBehaviors: {CountBehavior}.
	cbh := dp getBehavior: CountBehavior.
	self assert: ( cbh count ) equals: 0.
	dp hitWithContext: context.
	self assert: ( cbh count ) equals: 1.
	dp hitWithContext: context.
	self assert:  ( cbh count ) equals: 2.

]

{ #category : 'tests' }
DebugPointTest >> testObjectCentricBreakDebugPoint [

	| obj obj2 |
	obj := DummyTestClass new.
	obj2 := DummyTestClass new.
	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      on: node
		      forObject: obj.

	self should: [ dp hitWithContext: context ] raise: Break.
	self should: [ obj id: 2 ] raise: Break.
	self shouldnt: [ obj2 id: 2 ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testObjectCentricBreakDebugPointOnVariableRead [

	| obj obj2 |
	obj := DummyTestClass new.
	obj2 := DummyTestClass new.

	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      forObject: obj
		      onVariableReadNamed: #id.
	self should: [ obj id ] raise: Break.
	self shouldnt: [ obj2 id ] raise: Break.
	self shouldnt: [ obj id: 2 ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testObjectCentricBreakOnceOnVariable [

	| obj |
	obj := DummyTestClass new.

	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      forObject: obj
		      onVariableWriteNamed: #id
		      withBehaviors: { OnceBehavior }.
	self shouldnt: [ DummyTestClass new ] raise: Break.
	self should: [ obj id: 2 ] raise: Break.
	self shouldnt: [ obj id: 3 ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testObjectCentricWatchDebugPoint [

	| his obj |
	"watches the values of argument variable `anId`"
	obj := DummyTestClass basicNew.
	dp := DebugPointManager
		      installNew: WatchDebugPoint
		      forObject: obj
		      onVariableWriteNamed: #id.
	his := OrderedCollection new.

	"check empty history"
	self assertCollection: dp history equals: his.
	"check history after hitting dp"
	obj initialize.
	his addFirst: 0.
	self assertCollection: dp history equals: his.
	"once more"
	DummyTestClass new id: 2.
	self assertCollection: dp history equals: his.
	"once more"
	obj id: 'string'.
	his addFirst: 'string'.
	self assertCollection: dp history equals: his.
	"once more"
	obj id: 'string2'.
	his addFirst: 'string2'.
	self assertCollection: dp history equals: his.
	"check if limit is applied correctly"
	dp limit: 2.
	his removeLast.
	self assertCollection: dp history equals: his.
	"hitting does not change the history size anymore"
	obj id: 42.
	his addFirst: 42.
	his removeLast.
	self assertCollection: dp history equals: his
]

{ #category : 'tests' }
DebugPointTest >> testObjectCentricWatchOnceDebugPoint [

	| his obj |
	"watches the values of argument variable `anId`"
	obj := DummyTestClass new.
	dp := DebugPointManager
		      installNew: WatchDebugPoint
		      forObject: obj
		      onVariableWriteNamed: #id
		      withBehaviors: { OnceBehavior }.

	his := OrderedCollection new.

	"check empty history"
	self assertCollection: dp history equals: his.
	"check history after not hitting dp"
	DummyTestClass new id: 2.
	self assertCollection: dp history equals: his.
	"check history after hitting dp"
	obj id: 2.
	his addFirst: 2.
	self assertCollection: dp history equals: his.
	"once more"
	obj id: 'string'.
	self assertCollection: dp history equals: his
]

{ #category : 'tests' }
DebugPointTest >> testOnceBehavior [
	dp := DebugPointManager installNew: DebugPoint on: node.
	dp addBehavior: (OnceBehavior new).
	self assert: (dp enabled) equals: true.
	dp hitWithContext: context.
	self assert: (dp enabled) equals: false.


]

{ #category : 'tests' }
DebugPointTest >> testOnceBreakBehavior [
	dp := DebugPointManager installNew: BreakDebugPoint on: node withBehaviors: {OnceBehavior}.

	self assert: (dp enabled) equals: true.
	self should: [dp hitWithContext: context] raise: Break.
	self assert: (dp enabled) equals: false.
	self shouldnt: [dp hitWithContext: context] raise: Break.

]

{ #category : 'tests' }
DebugPointTest >> testOnceBreakOnVariable [

	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      inClass: DummyTestClass
		      onVariableWriteNamed: #id
		      withBehaviors: { OnceBehavior }.
	self should: [ DummyTestClass new ] raise: Break.
	self shouldnt: [ DummyTestClass basicNew id: 2 ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testOnceEvaluatesAfterConditionBehavior [

	| conditionBehavior |
	conditionBehavior := ConditionBehavior new condition:
		                     'corner = (300@200) '.
	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      on: node
		      withBehaviors: { OnceBehavior }.
	dp addBehavior: conditionBehavior.
	dp saveContext: context.

	self assert: dp enabled.
	self deny: conditionBehavior execute.

	self shouldnt: [ dp hitWithContext: context ] raise: Break.

	conditionBehavior condition: 'corner = (200 @ 200)'.

	self assert: dp enabled.
	self assert: conditionBehavior execute.

	self should: [ dp hitWithContext: context ] raise: Break.

	self deny: dp enabled.
	self assert: conditionBehavior execute.
	self shouldnt: [ dp hitWithContext: context ] raise: Break
]

{ #category : 'tests' }
DebugPointTest >> testScriptDebugPoint [

	"dp := DebugPointManager installNew: TranscriptDebugPoint On: node."
	dp := DebugPointManager installNew: DebugPoint on: node withBehaviors: {ScriptBehavior}.
	dp script: 'true'.
	self shouldnt: [dp hitWithContext: context] raise: Break.
	self shouldnt: [DummyTestClass new id:2 ] raise: Break.
	dp script: 'Break signal'.
	self should: [dp hitWithContext: context] raise: Break.
	self should: [DummyTestClass new id:2 ] raise: Break.
]

{ #category : 'tests' }
DebugPointTest >> testTestEnvironmentBehavior [

	| currentEnvironment |
	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      on: node
		      withBehaviors: { TestEnvironmentBehavior }.


	self should: [ dp hitWithContext: context ] raise: Break.

	currentEnvironment := CurrentExecutionEnvironment value.
	"To check if the test triggers when the execution environment is not a test"
	[
	CurrentExecutionEnvironment restoreDefault.
	self assert: CurrentExecutionEnvironment value isTest equals: false.
	self shouldnt: [ dp hitWithContext: context ] raise: Break ] ensure: [
		CurrentExecutionEnvironment value: currentEnvironment ]
]

{ #category : 'tests' }
DebugPointTest >> testTranscriptDebugPoint [

	"dp := DebugPointManager installNew: TranscriptDebugPoint On: node."
	dp := DebugPointManager installNew: DebugPoint on: node withBehaviors: {TranscriptBehavior}.
	dp text: 'string'.
	dp hitWithContext: context.
	Transcript stepGlobal .
	self assert: Transcript contents equals: 'string'
]

{ #category : 'tests' }
DebugPointTest >> testTwoDebugPointsOnSameTargetBothActivate [

	| his node2 |
	"watches the values of argument variable `anId`"
	node2 := (DummyTestClass methods select: [ :method |
		          method name = 'DummyTestClass>>#id:' ]) first ast children
		         second children first children first children first.
	dp := DebugPointManager installNew: WatchDebugPoint on: node2.
	dp2 := DebugPointManager installNew: WatchDebugPoint on: node2.
	his := OrderedCollection new.

	"check empty history"
	self assertCollection: dp history equals: his.
	self assertCollection: dp2 history equals: his.
	"check history after hitting dp"
	DummyTestClass new id: 2.
	his add: 2.
	self assertCollection: dp history equals: his.
	self assertCollection: dp2 history equals: his
]

{ #category : 'tests' }
DebugPointTest >> testWatchDebugPoint [

	| his node2 |
	"watches the values of argument variable `anId`"
	node2 := (DummyTestClass methods select: [ :method |
		          method name = 'DummyTestClass>>#id:' ]) first ast children
		         second children first children first children first.
	dp := DebugPointManager installNew: WatchDebugPoint on: node2.
	his := OrderedCollection new.

	"check empty history"
	self assertCollection: dp history equals: his.
	"check history after hitting dp"
	DummyTestClass new id: 2.
	his add: 2.
	self assertCollection: dp history equals: his.
	"once more"
	DummyTestClass new id: 'string'.
	his addFirst: 'string'.
	self assertCollection: dp history equals: his.
	"once more"
	DummyTestClass new id: 'string2'.
	his addFirst: 'string2'.
	self assertCollection: dp history equals: his.
	"check if limit is applied correctly"
	dp limit: 2.
	his removeLast.
	self assertCollection: dp history equals: his.
	"hitting does not change the history size anymore"
	DummyTestClass new id: 'string3'.
	his addFirst: 'string3'.
	his removeLast.
	self assertCollection: dp history equals: his
]
