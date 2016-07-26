requirements
testNew
	"self debug: #testNew"

	
	self should: [Symbol new: 5 ] raise: self defaultTestError. 