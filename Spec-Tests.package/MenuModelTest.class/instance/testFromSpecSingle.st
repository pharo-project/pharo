tests
testFromSpecSingle
	| menuDescription |
	menuDescription := PluggableMenuSpec withModel: nil.
	menuDescription add: 'item'.
	menuModel := MenuModel new.
	menuModel fromSpec: menuDescription.
	self assert: menuModel menuGroups size equals: 1.
	self assert: menuModel menuGroups first menuItems size equals: 1.
	self assert: menuModel menuGroups first menuItems first name equals: 'item'.
	window := menuModel openWithSpec