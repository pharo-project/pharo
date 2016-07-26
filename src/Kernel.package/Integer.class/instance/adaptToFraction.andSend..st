converting
adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert me to a Fraction."
	^ rcvr perform: selector with: (Fraction numerator: self denominator: 1)