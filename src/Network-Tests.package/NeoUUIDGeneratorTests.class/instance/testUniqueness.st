testing
testUniqueness
	| uuids count |
	count := 100000.
	uuids := Set new: count.
	count timesRepeat: [ uuids add: generator next ].
	self assert: uuids size equals: count