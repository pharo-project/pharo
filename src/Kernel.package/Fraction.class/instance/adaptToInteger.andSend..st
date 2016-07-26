converting
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Fraction."
	^ (Fraction numerator: rcvr denominator: 1) perform: selector with: self