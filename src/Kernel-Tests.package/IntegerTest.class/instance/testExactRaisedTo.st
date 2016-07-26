tests - mathematical functions
testExactRaisedTo
	"
	IntegerTest new testExactRaisedTo
	"
	self assert: (4 raisedTo: 1/2) classAndValueEquals: 2.
	self assert: (9 raisedTo: 1/2) classAndValueEquals: 3.
	self assert: (9 raisedTo: -1/2) classAndValueEquals: 1/3.
	self assert: (-1 raisedTo: 1/3) classAndValueEquals: -1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: (i squared raisedTo: 1/2) classAndValueEquals: i.
		self assert: (i negated squared raisedTo: 1/2) classAndValueEquals: i ].

	self assert: (8 raisedTo: 1/3) classAndValueEquals: 2.
	self assert: (27 raisedTo: 1/3) classAndValueEquals: 3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i.
		self assert: ((i negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i negated ].

	self assert: (4 raisedTo: 3/2) classAndValueEquals: 8.
	self assert: (8 raisedTo: 2/3) classAndValueEquals: 4.
	self assert: (8 raisedTo: -2/3) classAndValueEquals: 1/4.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i.
		self assert: ((i negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i ].

	self assert: (32 raisedTo: 3/5) classAndValueEquals: 8.
	self assert: (8 raisedTo: 5/3) classAndValueEquals: 32.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 5) raisedTo: 3/5) classAndValueEquals: i*i*i.
		self assert: ((i raisedTo: 3) raisedTo: 5/3) classAndValueEquals: i*i*i*i*i.
		self assert: ((i negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (i*i*i) negated.
		self assert: ((i negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (i*i*i*i*i) negated.

		self assert: ((i raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(i*i*i*i*i).

		self assert: ((i raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(i*i*i*i*i).

		"No exact result => Float result"
		self assert: ((i raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((i negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].