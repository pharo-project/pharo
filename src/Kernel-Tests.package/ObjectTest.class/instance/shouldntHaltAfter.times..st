assertions - halt
shouldntHaltAfter: aNumber times: aBlock
	
	self shouldntHaltWhen: [ aNumber timesRepeat: aBlock ].