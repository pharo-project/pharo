tests
testLayoutShouldComputeExtent
	"text is not empty, so layout should be not empty either"
	self assert: view layout extent x isZero not.
	self assert: view layout extent y isZero not.
	
