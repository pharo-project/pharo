"
A wathchpoint save the change of values of expressions. 
I save them as an oredered collection of  key val association where the key is the timestamp an val is the actual value of the expression.

To install a watchpoint in a node: 
Watchpoint in: aNode
"
Class {
	#name : #Watchpoint,
	#superclass : #Object,
	#instVars : [
		'tagname',
		'node',
		'link',
		'values',
		'recording'
	],
	#classVars : [
		'Watchpoints'
	],
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #accessing }
Watchpoint class >> allWatchpoints [
	^ Watchpoints ifNil: [ Watchpoints := WeakKeyDictionary new ]
]

{ #category : #cleanup }
Watchpoint class >> deleteAllHistory [

	self allWatchpoints valuesDo: [ :aWatchpoint| aWatchpoint deleteHistory].
]

{ #category : #cleanup }
Watchpoint class >> deleteHistoryFrom: aNode [

	(self allWatchpoints at: aNode ifAbsent:[^self]) deleteHistory
]

{ #category : #accessing }
Watchpoint class >> from: aNode [
	^self allWatchpoints at: aNode.
]

{ #category : #accessing }
Watchpoint class >> from: aNode ifAbsent: aBlock [
	^self allWatchpoints at: aNode ifAbsent: aBlock.
]

{ #category : #'instance creation' }
Watchpoint class >> in: aRBProgramNode [
	^self in: aRBProgramNode named: ''.
]

{ #category : #'instance creation' }
Watchpoint class >> in: aRBProgramNode named: aName [
	aRBProgramNode hasWatchpoint ifTrue: [ ^ aRBProgramNode watchpoint ].
	^self new 
		node: aRBProgramNode;
		tagname: aName;
		install.
]

{ #category : #cleanup }
Watchpoint class >> removeAll [
	self allWatchpoints values do: [ :aWatchpoint| aWatchpoint uninstall].
	Watchpoints := nil.
]

{ #category : #cleanup }
Watchpoint class >> removeFrom: aNode [

	(self allWatchpoints at: aNode ifAbsent: [ ^self ]) uninstall
]

{ #category : #adding }
Watchpoint >> addValue: anObject [
	values add: 
		(DateAndTime now -> anObject).
	^anObject.

]

{ #category : #accessing }
Watchpoint >> compiledMethod [ 
	^self node methodNode method
]

{ #category : #cleanup }
Watchpoint >> deleteHistory [
	values := OrderedCollection new.
]

{ #category : #initialization }
Watchpoint >> initialize [
	super initialize.
	values := OrderedCollection new.
	recording := true.
]

{ #category : #install }
Watchpoint >> install [

	node hasWatchpoint ifTrue: [ self error: 'Node already has a watchpoint'].
	
	link := MetaLink new 
				metaObject: self;
				selector: #addValue:;
				arguments: #(value);
				control: #after;
				condition: [ recording ];
				option: #(+ optionWeakAfter +optionAnnounce).
	node link: link.
	self class allWatchpoints at: node put: self.
]

{ #category : #recording }
Watchpoint >> isRecording [
	^recording
]

{ #category : #accessing }
Watchpoint >> node [
	^ node
]

{ #category : #accessing }
Watchpoint >> node: anObject [
	node := anObject
]

{ #category : #printing }
Watchpoint >> printLastValue [
	values ifNil: [ ^''].
	values ifEmpty: [ ^'not run' ].
	^values last value printStringLimitedTo: 5.
]

{ #category : #printing }
Watchpoint >> printOn: aStream [
	aStream nextPutAll: 'WP'.
	tagname isAllSeparators 
		ifFalse: [ 
			aStream nextPutAll: ' '.
			aStream nextPutAll: tagname ]. 
	aStream nextPutAll: ': '.
	aStream nextPutAll: (self printLastValue).
]

{ #category : #recording }
Watchpoint >> start [
	recording := true.
	link announceChange
]

{ #category : #recording }
Watchpoint >> stop [
	recording := false.
	link announceChange
]

{ #category : #accessing }
Watchpoint >> tagname [
	^ tagname
]

{ #category : #accessing }
Watchpoint >> tagname: anObject [
	tagname := anObject
]

{ #category : #install }
Watchpoint >> uninstall [
	node hasWatchpoint ifFalse: [ ^self ].
	link uninstall.
	self class allWatchpoints removeKey: node.
]

{ #category : #accessing }
Watchpoint >> values [
	^values.
]
