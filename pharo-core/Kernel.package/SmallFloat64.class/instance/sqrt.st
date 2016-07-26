mathematical functions
sqrt
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive."
	| exp guess eps delta |
	<primitive: 555>
	"Newton-Raphson"
	self <= 0.0
		ifTrue: [self = 0.0
				ifTrue: [^ 0.0]
				ifFalse: ["v Chg"
					^ DomainError signal: 'sqrt undefined for number less than zero.']].
	"first guess is half the exponent"
	exp := self exponent // 2.
	guess := self timesTwoPower: 0 - exp.
	"get eps value"
	eps := guess * Epsilon.
	eps := eps * eps.
	delta := self - (guess * guess) / (guess * 2.0).
	[delta * delta > eps]
		whileTrue: 
			[guess := guess + delta.
			delta := self - (guess * guess) / (guess * 2.0)].
	^ guess