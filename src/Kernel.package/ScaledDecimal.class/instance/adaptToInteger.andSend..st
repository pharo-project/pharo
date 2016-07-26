converting
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a ScaledDecimal."

	^(rcvr asScaledDecimal: scale) perform: selector with: self