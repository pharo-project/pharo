benchmarks
benchManyWeakGuys

	| object ann time |
	
	object := Object new.
	ann := Announcer new.
	
	50000 timesRepeat: [ (ann weak when: Announcement send: #value to: object) ].
	
	self assert: (ann numberOfSubscriptions =  50000).
	
	Smalltalk garbageCollect.
	
	time := [object := nil.
		Smalltalk garbageCollect. 
		 ]timeToRun.
	
	self assert: (ann numberOfSubscriptions = 0).
	^ time