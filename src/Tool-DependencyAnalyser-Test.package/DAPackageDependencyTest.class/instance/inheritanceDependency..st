running
inheritanceDependency: aClass
	^ (DAInheritanceDependency from: packageA to: packageB)
		theClass: aClass;
		superclass: nil;
		yourself