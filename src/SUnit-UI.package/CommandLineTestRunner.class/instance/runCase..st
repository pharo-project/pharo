running
runCase: aTestCase
	self increaseTestCount.
	self printTestCase: aTestCase.
	
	[[ aTestCase runCase. 
		self printPassOf: aTestCase ] 
		on: Halt , Error, TestFailure
		do: [ :err | self handleFailure: err of: aTestCase ]]
		on: TestSkip do: [ :skip| self handleSkip: skip of: aTestCase ]