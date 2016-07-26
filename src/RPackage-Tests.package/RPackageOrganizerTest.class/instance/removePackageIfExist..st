utility
removePackageIfExist: aName

	self packageOrganizer basicUnregisterPackageNamed: aName asSymbol.
		
	