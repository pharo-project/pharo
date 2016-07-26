tests
testEquals

	self assert: (3 to: 5) = #(3 4 5).
	self deny: (3 to: 5) = #(3 5).
	self deny: (3 to: 5) = #().

	self assert: #(3 4 5) = (3 to: 5).
	self deny: #(3 5) = (3 to: 5).
	self deny: #() = (3 to: 5).