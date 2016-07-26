tests
testEvents
	monitor enable.
	
	self deny: ((monitor log events collect: #class) includes: EpClassAddition).
	
	classFactory newClass.

	self assert: ((monitor log events collect: #class) includes: EpClassAddition)