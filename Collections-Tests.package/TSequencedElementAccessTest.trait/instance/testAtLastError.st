tests - element accessing
testAtLastError
	"self debug: #testAtLast"
	self 
		should: [ self moreThan4Elements atLast: self moreThan4Elements size + 1 ]
		raise: Error