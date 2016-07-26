converting
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Point."
	^ rcvr@rcvr perform: selector with: self