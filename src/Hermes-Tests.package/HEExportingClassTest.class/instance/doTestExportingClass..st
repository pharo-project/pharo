tests
doTestExportingClass: classUnderTest
	| new old nonExtensionMethodsClassSide nonExtensionMethods |
	old := HEClass for: classUnderTest.

	nonExtensionMethods := (classUnderTest localMethods reject: #isExtension)
		size.
	nonExtensionMethodsClassSide := (classUnderTest class localMethods
		reject: #isExtension) size.

	self assert: old methods size equals: nonExtensionMethods.
	self
		assert: old classSideMethods size
		equals: nonExtensionMethodsClassSide.

	old writeInto: writer.
	writer flush.

	new := HEClass readFrom: reader.
	self assert: old equals: new