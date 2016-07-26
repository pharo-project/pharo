tests - mathematical functions
testExactRaisedTo
	"
	FractionTest new testExactRaisedTo
	"
	| f |
	self assert: (4/9 raisedTo: 1/2) classAndValueEquals: 2/3.
	self assert: (9/4 raisedTo: 1/2) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f.
		f := b / a.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f ].

	self assert: (8/27 raisedTo: 1/3) classAndValueEquals: 2/3.
	self assert: (27/8 raisedTo: 1/3) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated.
		f := b / a.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated ].

	self assert: (4/9 raisedTo: 3/2) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 2/3) classAndValueEquals: 4/9.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		f := b / a.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f ].

	self assert: (32/243 raisedTo: 3/5) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 5/3) classAndValueEquals: 32/243.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f := a / b.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		self assert: ((f raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(f*f*f*f*f).
		self assert: ((f raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(f*f*f*f*f).

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat.

		f := b / a.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].