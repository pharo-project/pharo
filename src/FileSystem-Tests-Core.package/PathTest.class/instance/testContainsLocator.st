tests
testContainsLocator
	| ancestor descendent |
	ancestor := FileLocator imageDirectory resolve path.
	descendent := FileLocator image / 'griffle'.
	self deny: (ancestor contains: descendent).
	self deny: (descendent contains: ancestor)