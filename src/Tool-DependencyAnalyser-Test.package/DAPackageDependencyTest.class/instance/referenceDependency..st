running
referenceDependency: aClass
	^ (DAReferenceDependency from: packageA to: packageB)
		theClass: aClass;
		selector: nil;
		reference: nil;
		method: nil;
		yourself