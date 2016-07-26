converting
adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert it to a Float."
	^ rcvr asFloat perform: selector with: self