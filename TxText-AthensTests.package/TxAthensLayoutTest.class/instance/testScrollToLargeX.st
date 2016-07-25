tests
testScrollToLargeX
	"never scroll beyond layout"

	view scrollBy: 10000@0.
	
	self assert: view offset x <= (view layout width - view extent x).