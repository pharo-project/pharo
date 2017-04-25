tests
testNoDeadSubscriptions

	| dead |

	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	dead := SystemAnnouncer uniqueInstance 
		subscriptions subscriptions select: [ :sub | 
			sub subscriber isNil 
		].
	
	self assert: dead asArray equals: #()