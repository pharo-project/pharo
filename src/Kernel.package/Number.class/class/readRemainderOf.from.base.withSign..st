deprecated
readRemainderOf: integerPart from: aStream base: base withSign: sign 
	"Read optional fractional part and exponent or decimal scale, and return the final result"
	"Number readFrom: '3r-22.2'"

	| value fractionDigits fracpos fractionPart fraction pos v foundDecimal |
	value := integerPart.
	fractionDigits := 0.
	foundDecimal := false.
	(aStream peekFor: $.)
		ifTrue: ["<integer>.<fraction>"
			foundDecimal := true.
			(aStream atEnd not
					and: [aStream peek digitValue between: 0 and: base - 1])
				ifTrue: [fracpos := aStream position.
					fractionPart := Integer readFrom: aStream base: base.
					fraction := fractionPart asFloat
								/ (base raisedTo: aStream position - fracpos).
					fractionDigits := aStream position - fracpos.
					value := value asFloat + fraction]].

	pos := aStream position.
	(v := self readScaledDecimal: integerPart
			fractionPart: fractionPart
			digits: fractionDigits
			base: base
			sign: sign
			from: aStream)
		ifNil: [aStream position: pos]
		ifNotNil: [^ v "<number>s<scale>>"].

	pos := aStream position.
	(v := self readExponent: value base: base from: aStream)
		ifNil: [aStream position: pos.
			(foundDecimal and: [fractionDigits = 0])
				ifTrue: ["oops - just <integer>."
							aStream skip: -1.
							"un-gobble the period"
							^ value * sign]]
		ifNotNil: [value := v "<number>(e|d|q)<exponent>>"].

	(value isFloat
			and: [value = 0.0
					and: [sign = -1]])
		ifTrue: [^ Float negativeZero]
		ifFalse: [^ value * sign]