utility
createNewTraitNamed: aName inPackage: p
	
	| cls |
	cls := self createNewTraitNamed: aName.
	p addClassDefinition: cls.
	^ cls