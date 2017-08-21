as yet unclassified
doTestExportingTrait: traitUnderTest
	| new old nonExtensionMethodsClassSide nonExtensionMethods |
	old := HETrait for: traitUnderTest.

	nonExtensionMethods := (traitUnderTest localMethods reject: #isExtension) size.
	nonExtensionMethodsClassSide := (traitUnderTest theMetaClass localMethods reject: #isExtension) size.

	self assert: old methods size equals: nonExtensionMethods.
	self assert: old classSideMethods size equals: nonExtensionMethodsClassSide.

	old writeInto: writer.
	writer flush.

	new := HETrait readFrom: reader.
	self assert: old equals: new