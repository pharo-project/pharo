tests
testMock
	" test the mock setup - the mock objects do exactly the same as the original implementation, but we replace the asynchronous implementation by a synchronous "
	self assert: spotter class = GTMockSpotter.
	self assert: spotter newStep class = GTMockSpotterStep.
	self assert: spotter newStep asyncTask class = GTMockSpotterTask.