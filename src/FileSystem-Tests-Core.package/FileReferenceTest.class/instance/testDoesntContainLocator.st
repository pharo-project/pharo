tests
testDoesntContainLocator
	| ref |
	ref := filesystem * 'griffle'. 
	self deny: (ref contains: FileLocator image)