triggering
methodRepackaged: aMethod from: aPackage to: anotherPackage
	self announce: (MethodRepackaged
						methodRepackaged: aMethod
						oldPackage: aPackage 
						newPackage: anotherPackage)