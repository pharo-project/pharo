tests
testScrollToNegativeX
	"never scroll beyond layout"
	
	
	view scrollBy: -1000@0.
	
	self assert: view offset x = 0.