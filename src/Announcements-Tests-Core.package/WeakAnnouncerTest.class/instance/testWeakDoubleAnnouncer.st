tests
testWeakDoubleAnnouncer

	| a1 a2 o |
	
	a1 := Announcer new.
	a2 := Announcer new.
	
	o := Object new.
	
	self 
	assert: a1 subscriptions numberOfSubscriptions isZero;
	assert: a2 subscriptions numberOfSubscriptions isZero.
	
	a1 weak when: Announcement send: #abcdef to: o.
	a2 weak when: Announcement send: #abcdef to: o.	
	
	self 
	assert: a1 subscriptions numberOfSubscriptions = 1;
	assert: a2 subscriptions numberOfSubscriptions = 1.
	
	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	self 
	assert: a1 subscriptions numberOfSubscriptions = 1;
	assert: a2 subscriptions numberOfSubscriptions = 1.	

	o := nil.
	
	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	self 
	assert: a1 subscriptions numberOfSubscriptions isZero;
	assert: a2 subscriptions numberOfSubscriptions isZero.