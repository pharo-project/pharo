adapting
adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]