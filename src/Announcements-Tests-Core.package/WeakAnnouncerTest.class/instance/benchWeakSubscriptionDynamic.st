benchmarks
benchWeakSubscriptionDynamic

	" run me to see how expensive becomeForward: is (compared to #benchWeakSubscriptionStatic ) "
	| object ann |
	
	object := Object new.
	ann := Announcer new.
	
	^ [
		1000 timesRepeat: [ (ann when: Announcement send: #value to: object) makeWeak ].
	] timeToRun