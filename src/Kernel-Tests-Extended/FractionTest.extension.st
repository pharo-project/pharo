Extension { #name : #FractionTest }

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testDegreeCos [

	(4 / 3) degreeCos.
	-361 / 3 to: 359 / 3 do: [ :i | self assert: (i degreeCos closeTo: i degreesToRadians cos) ]
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testDegreeSin [

	(4 / 3) degreeSin.
	-361 / 3 to: 359 / 3 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ]
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testExactRaisedTo [
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
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testExactSqrt [
	"
	FractionTest new testExactSqrt
	"
	| f |
	self assert: (4/9) sqrt classAndValueEquals: 2/3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :i :j |
		f := i / j.
		self assert: f squared sqrt classAndValueEquals: f.
		f := j / i.
		self assert: f squared sqrt classAndValueEquals: f ]
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testFloorLog [
	self assert: (1 / 100 floorLog: 10) equals: -2.
	self assert: ((2 raisedTo: Float emax + 11) / 3 floorLog: 10) = ((Float emax + 11) * 2 log - 3 log) floor description: 'Fraction>>log should not overflow'.
	self
		assert: (3 / (2 raisedTo: Float precision - Float emin) floorLog: 10) = ((Float emin - Float precision) * 2 log + 3 log) floor
		description: 'Fraction>>log should not underflow'
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testInexactRaisedTo [
	"
	FractionTest new testInexactRaisedTo
	"

	self assert: (((1 << 1024) + 1) / ((1 << 1024) + 3) raisedTo: 1 / 3) equals: 1.0.
	self assert: ((((1 << 1024) + 1) / ((1 << 1024) + 3)) negated raisedTo: 1 / 3) equals: -1.0
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testInexactSqrt [
	"
	FractionTest new testInexactSqrt
	"

	self assert: (((1 << 1024) + 1) / ((1 << 1024) + 3)) sqrt equals: 1.0
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testLn [
	self assert: ((1/100) ln closeTo: -2 * 10 ln).
	self assert: (((2 raisedTo: Float emax + 11)/3) ln closeTo: (Float emax + 11)*2 ln - 3 ln) description: 'Fraction>>ln should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) ln closeTo: (Float emin - Float precision)*2 ln + 3 ln) description: 'Fraction>>ln should not underflow'
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testLog [
	self assert: ((1/100) log closeTo: -2).
	self assert: (((2 raisedTo: Float emax + 11)/3) log closeTo: (Float emax + 11)*2 log - 3 log) description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) log closeTo: (Float emin - Float precision)*2 log + 3 log) description: 'Fraction>>log should not underflow'
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testNthRoot [
	self assert: ((-2 raisedTo: 35) / (3 raisedTo: 20) raisedTo: 1/5) equals: (-2 raisedTo: 7) / (3 raisedTo: 4).
	self assert: (1 / (1 << 2000) raisedTo: 1/100) equals: 1 / (1 << 20)
]

{ #category : #'*Kernel-Tests-Extended' }
FractionTest >> testSqrtErrorConditions [
	"
	FractionTest new testSqrtErrorConditions
	"
	self should: [ (-1/4) sqrt ] raise: DomainError.
	self should: [ ((1 << 1024 + 1) / (1 << 1024 + 3)) negated sqrt ] raise: DomainError
]
