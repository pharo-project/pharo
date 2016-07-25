tests
testEachSelectorExists
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self testedTutorial respondsTo: aSelector) 
	]