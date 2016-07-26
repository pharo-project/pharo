converting
adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert it to a ScaledDecimal."

	^(rcvr asScaledDecimal: scale) perform: selector with: self