tests
testAsInterval
	"This is the same as newFrom:"
	"self run: #testAsIntervaltestAsInterval"
	self assert: (#(1 2 3) as: Interval) = (1 to: 3).
	self assert: (#(33 5 -23) as: Interval) = (33 to: -23 by: -28).
	self assert: (#[2 4 6] as: Interval) = (2 to: 6 by: 2).

	self should: [#(33 5 -22) as: Interval]
		raise: Error
		description: 'This is not an arithmetic progression'
