assertions
assertText: aTextualObject do: aBlock
	| result |
	" self: emulate a view. Forget about a separate GTSpotterMockView. kiss :) "
	self shouldnt: [ result := spotter setText: aTextualObject from: self. ] raise: Error.
	
	self assert: result.
	self deny: spotter hasSteps not.
	self deny: spotter steps isEmpty.
	
	self shouldnt: [ aBlock value ] raise: Error.
	
	