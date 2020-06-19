Class {
	#name : #WatchTest,
	#superclass : #TestCase,
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #adding }
WatchTest >> testAddSameWatchSameNode [
	| node watchpoint watchpoint2 |
	node := (ReflectivityExamples >> #exampleAssignment) ast body children first.
	watchpoint := Watch in: node.
	watchpoint2 := Watch in: node.

	self assert: watchpoint equals: watchpoint2.

	watchpoint uninstall.

	self deny: node hasWatch
]

{ #category : #adding }
WatchTest >> testAddWatch [
	| node watchpoint |
	node := (ReflectivityExamples >> #exampleAssignment) ast body children first.
	watchpoint := Watch in: node.

	self assert: node hasWatch.
	self assert: (Watch allWatches at: node) equals: watchpoint.

	watchpoint uninstall.
	self deny: node hasWatch.
	self deny: (Watch allWatches includesKey: node)
]

{ #category : #adding }
WatchTest >> testAddWatchSameNode [
	| node watchpoint watchpoint2 |
	node := (ReflectivityExamples >> #exampleAssignment) ast body children first.
	watchpoint := Watch in: node.
	watchpoint2 := Watch in: node.

	self assert: watchpoint equals: watchpoint2.

	watchpoint uninstall.

	self deny: node hasWatch
]

{ #category : #'deleting history' }
WatchTest >> testDeleteAllHistory [
	| node1 node2 watchpoint1 watchpoint2 |
	node1 := (ReflectivityExamples >> #exampleAssignmentIvar:) ast body statements first.
	watchpoint1 := Watch in: node1.
	node2 := (ReflectivityExamples >> #exampleAssignmentIvar) ast body statements first.
	watchpoint2 := Watch in: node2.

	ReflectivityExamples new exampleAssignmentIvar: 2.
	ReflectivityExamples new exampleAssignmentIvar.

	self assert: watchpoint1 values first value equals: 2.
	self assert: watchpoint2 values first value equals: 3.

	Watch deleteAllHistory.

	self assertEmpty: watchpoint1 values.
	self assertEmpty: watchpoint2 values.

	watchpoint1 uninstall.
	watchpoint2 uninstall
]

{ #category : #'deleting history' }
WatchTest >> testDeleteHistory [
	| node watchpoint |
	node := (ReflectivityExamples >> #exampleAssignmentIvar:) ast body children first.
	watchpoint := Watch in: node.
	ReflectivityExamples new exampleAssignmentIvar: 1.
	ReflectivityExamples new exampleAssignmentIvar: 2.

	self assert: watchpoint values first value equals: 1.
	self assert: watchpoint values second value equals: 2.

	watchpoint deleteHistory.
	self assertEmpty: watchpoint values.

	watchpoint uninstall
]

{ #category : #'deleting history' }
WatchTest >> testDeleteHistoryFromNode [
	| node watchpoint |
	node := (ReflectivityExamples >> #exampleAssignmentIvar:) ast body children first.
	watchpoint := Watch in: node.
	ReflectivityExamples new exampleAssignmentIvar: 1.
	ReflectivityExamples new exampleAssignmentIvar: 2.

	self assert: watchpoint values first value equals: 1.
	self assert: watchpoint values second value equals: 2.

	Watch deleteHistoryFrom: node.
	self assertEmpty: watchpoint values.

	watchpoint uninstall
]

{ #category : #values }
WatchTest >> testSaveOneValue [
	| node watchpoint |
	node := (ReflectivityExamples >> #exampleAssignmentIvar) ast body children first.
	watchpoint := Watch in: node.
	ReflectivityExamples new exampleAssignmentIvar.

	self assert: watchpoint values first value equals: 3.

	watchpoint uninstall
]

{ #category : #values }
WatchTest >> testSaveValues [
	| node watchpoint |
	node := (ReflectivityExamples >> #exampleAssignmentIvar:) ast body children first.
	watchpoint := Watch in: node.
	ReflectivityExamples new exampleAssignmentIvar: 1.
	ReflectivityExamples new exampleAssignmentIvar: 2.

	self assert: watchpoint values first value equals: 1.
	self assert: watchpoint values second value equals: 2.

	watchpoint uninstall
]

{ #category : #values }
WatchTest >> testSendMessage [
	| node watchpoint |
	node := (ReflectivityExamples >> #exampleMessageSend) ast body children first.
	watchpoint := Watch in: node.
	ReflectivityExamples new exampleMessageSend.

	self assert: watchpoint values first value class equals: ReflectivityExamples.

	watchpoint uninstall
]

{ #category : #recording }
WatchTest >> testStopRecording [
	| node watchpoint dummy |
	node := (ReflectivityExamples >> #exampleAssignmentIvar:) ast body children first.
	watchpoint := Watch in: node.
	dummy := ReflectivityExamples new.

	dummy exampleAssignmentIvar: 1.
	dummy exampleAssignmentIvar: 2.
	self assert: watchpoint values size equals: 2.
	self assert: watchpoint values second value equals: 2.

	watchpoint stop.
	dummy exampleAssignmentIvar: 3.
	self assert: watchpoint values size equals: 2.

	watchpoint start.
	dummy exampleAssignmentIvar: 4.
	self assert: watchpoint values size equals: 3.
	self assert: watchpoint values third value equals: 4.

	watchpoint uninstall.

	self deny: node hasWatch
]

{ #category : #values }
WatchTest >> testTimestamp [
	|node watchpoint|
	
	node := (ReflectivityExamples>>#exampleAssignmentIvar:) ast body children first.
	watchpoint := Watch in: node.
	
	1 to: 10 do:[:i| ReflectivityExamples new exampleAssignmentIvar: i].
	1 to: 9 do: [:i|  
		|older newer|
		older := watchpoint values at: i.
		newer := watchpoint values at: i + 1.
		self assert: (older key <= newer key).
	]. 
	
	watchpoint uninstall.
	
]

{ #category : #adding }
WatchTest >> testWatchTwice [
	|node watchpoint|
	
	node := (ReflectivityExamples>>#exampleAssignment) ast body statements first.
	watchpoint := Watch in: node.
	
	self should: [watchpoint install] raise: Error.
	
	watchpoint uninstall.
]
