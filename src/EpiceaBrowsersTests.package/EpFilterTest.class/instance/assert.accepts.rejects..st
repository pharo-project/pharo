running
assert: aFilter accepts: anEvent rejects: anotherEvent
	self assert: (aFilter accepts: anEvent).
	self deny: (aFilter accepts: anotherEvent).