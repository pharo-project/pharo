events handling
doubleClickOnPackage
	self selectedPackage ifNil: [ ^ self ] ifNotNil: [ :package | self model class openOnPackage: package ]