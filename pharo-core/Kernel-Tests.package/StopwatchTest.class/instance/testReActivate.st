tests
testReActivate

	| sw |
	sw := Stopwatch new.
	sw 
		activate;
		suspend;
		reActivate.
	
	self 
		assert: (sw isActive).
