assertions - halt
shouldHaltAfter: aNumber times: aBlock
	
	self shouldHaltWhen: [ aNumber timesRepeat: aBlock ].