adding
addReferenceDependencies: aPackage
	(self methodsFor: aPackage) 
		do: [ :method | self findReferencesIn: method for: aPackage ]