"
I am a meta-object that can be put on any AST node to count execution

To install a watchpoint in a node: 
	ExecutionCounter installOn: aNode
	
Class CounterIconStyler then renders an icon in the editor, mouse over it to see the current counter value.
"
Class {
	#name : #ExecutionCounter,
	#superclass : #Object,
	#instVars : [
		'node',
		'link',
		'count'
	],
	#classVars : [
		'Counters'
	],
	#category : #'Reflectivity-Tools-Watchpoints'
}

{ #category : #accessing }
ExecutionCounter class >> allCounters [
	^ Counters ifNil: [ Counters := WeakKeyDictionary new ]
]

{ #category : #'instance creation' }
ExecutionCounter class >> installOn: aRBProgramNode [
	^self new
		node: aRBProgramNode;
		install.
]

{ #category : #cleanup }
ExecutionCounter class >> removeAll [
	self allCounters values do: [ :aWatchpoint| aWatchpoint uninstall].
	Counters := nil.
]

{ #category : #cleanup }
ExecutionCounter class >> removeFrom: aNode [

	(self allCounters at: aNode ifAbsent: [ ^self ]) uninstall
]

{ #category : #cleanup }
ExecutionCounter class >> resetAll [
	self allCounters values do: [ :counter | counter reset ].
	Counters := nil
]

{ #category : #accessing }
ExecutionCounter >> count [
	^ count
]

{ #category : #counter }
ExecutionCounter >> increase [
	count := count + 1
]

{ #category : #initialization }
ExecutionCounter >> initialize [
	super initialize.
	count := 0.
]

{ #category : #'add/remove' }
ExecutionCounter >> install [

	link := MetaLink new 
				metaObject: self;
				selector: #increase.
	node link: link.
	self class allCounters at: node put: self.
]

{ #category : #accessing }
ExecutionCounter >> node [
	^ node
]

{ #category : #accessing }
ExecutionCounter >> node: anObject [
	node := anObject
]

{ #category : #counter }
ExecutionCounter >> reset [
	count := 0
]

{ #category : #'add/remove' }
ExecutionCounter >> uninstall [
	node hasExecutionCounter ifFalse: [ ^self ].
	link uninstall.
	self class allCounters removeKey: node.
]
