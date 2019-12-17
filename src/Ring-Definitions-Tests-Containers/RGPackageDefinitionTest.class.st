"
SUnit tests for packages
"
Class {
	#name : #RGPackageDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Containers-Base'
}

{ #category : #testing }
RGPackageDefinitionTest >> testAddingClass [
	| newPackage newClass |
	newPackage := RGPackageDefinition named: #'Collections-Sequenceable'.
	newClass := RGClassDefinition named: #OrderedCollection.
	newPackage addClass: newClass.

	self assert: newPackage hasClasses.
	self assert: newClass package identicalTo: newPackage.
	self assert: (newPackage classNamed: #OrderedCollection) identicalTo: newClass
]

{ #category : #testing }
RGPackageDefinitionTest >> testAddingPackage [
	| newPackage childPackage |
	newPackage := RGPackageDefinition named: #Collections.
	newPackage addPackageNamed: #'Collections-Sequenceable'.

	self assert: (newPackage includesPackageNamed: #'Collections-Sequenceable').
	self assert: newPackage packages size equals: 1.

	childPackage := newPackage packageNamed: #'Collections-Sequenceable'.
	self assert: childPackage parent equals: newPackage parent.
	self assert: childPackage package equals: newPackage
]

{ #category : #testing }
RGPackageDefinitionTest >> testNewPackage [
	| newPackage |
	newPackage := RGPackageDefinition named: 'Collections-Sequenceable'.

	self assert: newPackage isPackage.
	self assert: newPackage name equals: 'Collections-Sequenceable'.
	self assertEmpty: newPackage classes.
	self assertEmpty: newPackage traits.
	self assertEmpty: newPackage packages.
	self assert: newPackage package isNil.
	self assert: newPackage parent identicalTo: Smalltalk globals
]
