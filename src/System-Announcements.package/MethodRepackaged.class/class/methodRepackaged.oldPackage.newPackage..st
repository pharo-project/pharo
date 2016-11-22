instance creation
methodRepackaged: aMethod oldPackage: oldPackage newPackage: newPackage
	^self new
			method: aMethod;
			oldPackage: oldPackage;
			newPackage: newPackage;
			yourself.