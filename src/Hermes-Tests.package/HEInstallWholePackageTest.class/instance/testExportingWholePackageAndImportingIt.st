tests
testExportingWholePackageAndImportingIt
	| hePackage readPackage newPackage oldPackage obj obj2 |
	oldPackage := RPackageOrganizer default packageNamed: #'Hermes-Tests-External-Package'.
	hePackage := HEPackage for: oldPackage.
	hePackage writeInto: writer.
	writer flush.

	readPackage := HEPackage readFrom: reader.
	self changeClassesNames: readPackage.

	builder installPackage: readPackage.
	newPackage := RPackageOrganizer default packageNamed: #'Hermes-Tests-External-Package2'.

	self assert: newPackage classes size equals: oldPackage classes size.

	newClass := Smalltalk at: #AClassToBeExtractedFactory2.
	obj := newClass new.
	obj2 := obj anObject.

	self assert: obj2 class equals: (Smalltalk at: #AClassToBeExtracted2)