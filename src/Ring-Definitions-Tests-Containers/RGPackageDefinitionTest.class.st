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
