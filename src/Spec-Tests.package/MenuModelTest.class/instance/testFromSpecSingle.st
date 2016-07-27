tests
testFromSpecSingle
	| menuDescription |
	menuDescription := PluggableMenuSpec withModel: nil.
	menuDescription add: 'item'.
	testedInstance fromSpec: menuDescription.
	self assert: testedInstance menuGroups size equals: 1.
	self assert: testedInstance menuGroups first menuItems size equals: 1.
	self assert: testedInstance menuGroups first menuItems first name equals: 'item'.
	self openInstance