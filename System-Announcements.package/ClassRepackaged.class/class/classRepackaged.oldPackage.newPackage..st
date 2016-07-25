instance creation
classRepackaged: aClass oldPackage: oldPackage newPackage: newPackage
	^self new
			classRepackaged: aClass;
			oldPackage: oldPackage;
			newPackage: newPackage;
			yourself.