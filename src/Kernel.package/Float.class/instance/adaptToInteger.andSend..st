converting
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Float."
	^ rcvr asFloat perform: selector with: self