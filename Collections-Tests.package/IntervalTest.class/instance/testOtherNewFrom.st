tests
testOtherNewFrom

	self assert: (Interval newFrom: #(1 2 3 )) = (1 to: 3).
	self assert: (Interval newFrom: #(33  5 -23 )) = (33 to: -23 by: -28).
	self should: [(Interval newFrom: #(33  5 -22 ))] raise: Error.
	self assert: (#(33  5 -23) as: Interval) = (33 to: -23 by: -28).
	self should: [( #(33  5 -22 ) as: Interval)] raise: Error.
	
	self assert: ( (-4 to: -12 by: -1) as: Interval) = (-4 to: -12 by: -1).
	self assert: ( Interval newFrom: (1 to: 1)) = (1 to: 1).
	self assert: ( Interval newFrom: (1 to: 0)) = (1 to: 0).
	self assert: (#(1) as: Interval) = (1 to: 1).
	self assert: (#() as: Interval) = (1 to: 0).