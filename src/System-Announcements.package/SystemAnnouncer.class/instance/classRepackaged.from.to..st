triggering
classRepackaged: aClass from: aPackage to: anotherPackage
	self announce: (ClassRepackaged
						classRepackaged: aClass
						oldPackage: aPackage 
						newPackage: anotherPackage)