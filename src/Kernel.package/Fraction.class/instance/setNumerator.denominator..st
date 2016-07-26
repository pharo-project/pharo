private
setNumerator: n denominator: d

	d = 0
		ifTrue: [^(ZeroDivide dividend: n) signal]
		ifFalse: 
			[numerator := n asInteger.
			denominator := d asInteger abs. "keep sign in numerator"
			d < 0 ifTrue: [numerator := numerator negated]]