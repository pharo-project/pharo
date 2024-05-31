"
SUnit tests for packages
"
Class {
	#name : 'RGPackageDefinitionTest',
	#superclass : 'TestCase',
	#category : 'Ring-Definitions-Tests-Containers-Base',
	#package : 'Ring-Definitions-Tests-Containers',
	#tag : 'Base'
}

{ #category : 'testing' }
RGPackageDefinitionTest >> testAddingClass [
	| newPackage newClass |
	newPackage := RGPackageDefinition named: #'Collections-Sequenceable'.
	newClass := RGClassDefinition named: #OrderedCollection.
	newPackage addClass: newClass.

	self assert: newPackage hasClasses.
	self assert: newClass package identicalTo: newPackage.
	self assert: (newPackage classNamed: #OrderedCollection) identicalTo: newClass
]

{ #category : 'tests' }
RGPackageDefinitionTest >> testCategoryMatching [

	self assert: ('Compression-Archives' isCategoryOf: 'Compression').
	self deny: ('Compression' isCategoryOf: 'Compression-Archives').
	self assert: ('Compression' isCategoryOf: 'Compression').
	self assert: ('Compression-Arh' isCategoryOf: 'Compression').
	self deny: ('XXXX' isCategoryOf: 'Compression-Archives')
]

{ #category : 'testing' }
RGPackageDefinitionTest >> testNewPackage [
	| newPackage |
	newPackage := RGPackageDefinition named: 'Collections-Sequenceable'.

	self assert: newPackage isPackage.
	self assert: newPackage name equals: 'Collections-Sequenceable'.
	self assertEmpty: newPackage classes.
	self assertEmpty: newPackage traits.
	self assert: newPackage parent identicalTo: Smalltalk globals
]
