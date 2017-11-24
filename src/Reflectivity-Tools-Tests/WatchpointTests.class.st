Class {
	#name : #WatchpointTests,
	#superclass : #TestCase,
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #adding }
WatchpointTests >> testAddSameWatchpointSameNode [
	|node watchpoint watchpoint2|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	watchpoint2 := Watchpoint in: node.
	
	self assert: (watchpoint = watchpoint2).
	
	watchpoint uninstall.
	
	self deny: (node hasWatchpoint)
	
]

{ #category : #adding }
WatchpointTests >> testAddWatchpoint [
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	
	self assert: node hasWatchpoint.
	self assert: (Watchpoint allWatchpoints at: node) = watchpoint.
	
	watchpoint uninstall.
	self deny: node hasWatchpoint.
	self deny: (Watchpoint allWatchpoints includesKey: node). 
	
]

{ #category : #adding }
WatchpointTests >> testAddWatchpointsSameNode [
	|node watchpoint watchpoint2|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	watchpoint2 := Watchpoint in: node.
	
	self assert: (watchpoint = watchpoint2).
	
	watchpoint uninstall.
	
	self deny: (node hasWatchpoint)
	
]

{ #category : #'deleting history' }
WatchpointTests >> testDeleteAllHistory [
	|node1 node2 watchpoint1 watchpoint2|
	
	node1 := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint1 := Watchpoint in: node1.
	node2 := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint2 := Watchpoint in: node2.
	
	WPDummy new exampleAssignment: 2.
	WPDummy new exampleAssignment.
		
	self assert: (watchpoint1 values first value = 2).
	self assert: (watchpoint2 values first value = 1).
	
	Watchpoint deleteAllHistory.
	
	self assert: watchpoint1 values isEmpty.
	self assert: watchpoint2 values isEmpty.
	
	watchpoint1 uninstall.
	watchpoint2 uninstall.
	
]

{ #category : #'deleting history' }
WatchpointTests >> testDeleteHistory [
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleAssignment: 1.
	WPDummy new exampleAssignment: 2.
	
	self assert: (watchpoint values first value = 1).
	self assert: (watchpoint values second value = 2).
	
	watchpoint deleteHistory.
	self assert: watchpoint values isEmpty.
	
	watchpoint uninstall.
	
]

{ #category : #'deleting history' }
WatchpointTests >> testDeleteHistoryFromNode [
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleAssignment: 1.
	WPDummy new exampleAssignment: 2.
	
	self assert: (watchpoint values first value = 1).
	self assert: (watchpoint values second value = 2).
	
	Watchpoint deleteHistoryFrom: node.
	self assert: watchpoint values isEmpty.
	
	watchpoint uninstall.
	
]

{ #category : #values }
WatchpointTests >> testSaveOneValue [
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleAssignment.
	
	self assert: (watchpoint values first value = 1).
	
	watchpoint uninstall.
	
]

{ #category : #values }
WatchpointTests >> testSaveValues [
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleAssignment: 1.
	WPDummy new exampleAssignment: 2.
	
	self assert: (watchpoint values first value = 1).
	self assert: (watchpoint values second value = 2).
	
	watchpoint uninstall.
	
]

{ #category : #values }
WatchpointTests >> testSendMessage [
	|node watchpoint|
	
	node := (WPDummy>>#exampleMessageSend) ast body children first.
	watchpoint := Watchpoint in: node.
	WPDummy new exampleMessageSend.
	
	self assert: (watchpoint values first value = 5).
	
	watchpoint uninstall.
	
]

{ #category : #recording }
WatchpointTests >> testStopRecording [
	|node watchpoint dummy|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	dummy := WPDummy new.
	
	dummy exampleAssignment: 1.
	dummy exampleAssignment: 2.
	self assert: (watchpoint values size = 2).
	self assert: (watchpoint values second value = 2).
	
	watchpoint stop.
	dummy exampleAssignment: 3.
	self assert: (watchpoint values size = 2).
	
	watchpoint start.
	dummy exampleAssignment: 4.
	self assert: (watchpoint values size = 3).
	self assert: (watchpoint values third value = 4).
	
	watchpoint uninstall.
	
	self deny: (node hasWatchpoint)
	
]

{ #category : #values }
WatchpointTests >> testTimestamp [
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment:) ast body children first.
	watchpoint := Watchpoint in: node.
	
	1 to: 10 do:[:i| WPDummy new exampleAssignment: i].
	1 to: 9 do: [:i|  
		|older newer|
		older := watchpoint values at: i.
		newer := watchpoint values at: i + 1.
		self assert: (older key <= newer key).
	]. 
	
	watchpoint uninstall.
	
]

{ #category : #adding }
WatchpointTests >> testWatchpointTwice [
	|node watchpoint|
	
	node := (WPDummy>>#exampleAssignment) ast body children first.
	watchpoint := Watchpoint in: node.
	
	self should: [watchpoint install] raise: Error.
	
	watchpoint uninstall.
]
