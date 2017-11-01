Class {
	#name : #HEExportingTraitTest,
	#superclass : #HEAbstractBinaryClassBuilderTest,
	#category : 'Hermes-Tests'
}

{ #category : #'as yet unclassified' }
HEExportingTraitTest >> doTestExportingTrait: traitUnderTest [
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
]

{ #category : #tests }
HEExportingTraitTest >> testExportingTrait [
	self doTestExportingTrait: THEOneTestTrait.
]

{ #category : #tests }
HEExportingTraitTest >> testExportingTraitUsingOther [
	self doTestExportingTrait: THEATraitUsingOthers.
]
