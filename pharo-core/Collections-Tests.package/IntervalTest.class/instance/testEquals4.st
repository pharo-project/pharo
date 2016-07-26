tests
testEquals4

	self assert: (3 to: 5 by: 2) = #(3 5).
	self deny: (3 to: 5 by: 2) = #(3 4 5).
	self deny: (3 to: 5 by: 2) = #().

	self assert: #(3 5) = (3 to: 5 by: 2).
	self deny: #(3 4 5) = (3 to: 5 by: 2).
	self deny: #() = (3 to: 5 by: 2).