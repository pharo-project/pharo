tests
testEachSelectorExists
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self tutorial respondsTo: aSelector) 
	]