menus behavior
addClass
	| package |
	package := self selectedPackage 
		ifNil: [ RPackageOrganizer default packageNamed: RPackage defaultPackageName ].
	self addClassIn: package.