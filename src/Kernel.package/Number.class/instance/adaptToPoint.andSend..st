converting
adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a Point, convert me to a Point."
	^ rcvr perform: selector with: self@self