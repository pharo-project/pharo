converting
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with a Integer, convert us and evaluate exprBlock."
	^ self subclassResponsibility