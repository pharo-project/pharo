mathematical functions
gcd: anInteger
	"See Knuth, Vol 2, 4.5.2, Algorithm L"
	"Initialize"
	| higher u v k uHat vHat a b c d vPrime vPrimePrime q t |
	higher := SmallInteger maxVal highBit.
	u := self abs max: (v := anInteger abs).
	v := self abs min: v.
	[v class == SmallInteger]
		whileFalse: 
			[(uHat := u bitShift: (k := higher - u highBit)) class == SmallInteger
				ifFalse: 
					[k := k - 1.
					uHat := uHat bitShift: -1].
			vHat := v bitShift: k.
			a := 1.
			b := 0.
			c := 0.
			d := 1.
			"Test quotient"
			[(vPrime := vHat + d) ~= 0
				and: [(vPrimePrime := vHat + c) ~= 0 and: [(q := uHat + a // vPrimePrime) = (uHat + b // vPrime)]]]
				whileTrue: 
					["Emulate Euclid"
					c := a - (q * (a := c)).
					d := b - (q * (b := d)).
					vHat := uHat - (q * (uHat := vHat))].
			"Multiprecision step"
			b = 0
				ifTrue: 
					[v := u rem: (u := v)]
				ifFalse: 
					[t := u * a + (v * b).
					v := u * c + (v * d).
					u := t]].
	^ v gcd: u