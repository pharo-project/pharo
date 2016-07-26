tests
testRemoveAllPages
	tabs removePage: tabs pages first.
	tabs removePage: tabs pages first.
	self assert: tabs pages isEmpty.
	self assert: tabs contentMorph submorphs isEmpty