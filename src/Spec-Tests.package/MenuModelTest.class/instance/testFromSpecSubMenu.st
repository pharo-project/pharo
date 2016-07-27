tests
testFromSpecSubMenu
	| root subMenuRoot parent child subMenu |
	root := PluggableMenuSpec withModel: nil.
	parent := root add: 'parent'.
	subMenuRoot := PluggableMenuSpec withModel: nil.
	child := subMenuRoot add: 'child'.
	parent subMenu: subMenuRoot.
	testedInstance fromSpec: root.
	subMenu := testedInstance menuGroups first menuItems first subMenu.
	self assert: subMenu menuGroups size equals: 1.
	self assert: subMenu menuGroups first menuItems size equals: 1.
	self assert: subMenu menuGroups first menuItems first name equals: 'child'.
	self openInstance