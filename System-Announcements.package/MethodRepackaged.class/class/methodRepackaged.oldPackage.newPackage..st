instance creation
methodRepackaged: aMethod oldPackage: oldPackage newPackage: newPackage
	^self new
			methodRepackaged: aMethod;
			oldPackage: oldPackage;
			newPackage: newPackage;
			yourself.