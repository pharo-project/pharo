tests
"protocol: tests"
testInstanceCreation
	self shouldnt: [ spotter := GTMockSpotter new ] raise: Error.
	self shouldnt: [ 
		spotter := GTMockSpotter new
			exceptionHandler: GTSpotterDebuggingExceptionHandler new;
			yourself ] raise: Error.
	
	self assert: spotter notNil.
	self assert: spotter steps isEmpty.
	self deny: spotter hasMultipleSteps