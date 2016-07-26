utility
createNewClassNamed: aName inPackage: p
	
	| cls |
	cls := self createNewClassNamed: aName.
	p addClassDefinition: cls.
	^ cls