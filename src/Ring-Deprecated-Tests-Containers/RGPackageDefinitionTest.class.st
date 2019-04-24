"
SUnit tests for packages
"
Class {
	#name : #RGPackageDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Deprecated-Tests-Containers-Base'
}

{ #category : #deprecation }
RGPackageDefinitionTest class >> isDeprecated [
	^true
]

{ #category : #testing }
RGPackageDefinitionTest >> testAddingClass [
	| newPackage newClass |
	
	newPackage:= RGPackageDefinition named: #'Collections-Sequenceable'.
	newClass:= RGClassDefinition named: #OrderedCollection.
	newPackage addClass: newClass.
	
	self assert: (newPackage hasClasses).
	self assert: (newClass package == newPackage).
	self assert: ((newPackage classNamed: #OrderedCollection) == newClass)
]

{ #category : #testing }
RGPackageDefinitionTest >> testAddingPackage [
	| newPackage childPackage |
	
	newPackage:= RGPackageDefinition named: #'Collections'.
	newPackage addPackageNamed: #'Collections-Sequenceable'.
		
	self assert: (newPackage includesPackageNamed: #'Collections-Sequenceable').
	self assert: (newPackage packages size = 1).
	
	childPackage:= newPackage packageNamed: #'Collections-Sequenceable'.
	self assert: (childPackage parent = newPackage parent).
	self assert: (childPackage package = newPackage).
]

{ #category : #testing }
RGPackageDefinitionTest >> testNewPackage [
	| newPackage |
	newPackage := RGPackageDefinition named: 'Collections-Sequenceable'.

	self assert: newPackage isPackage.
	self assert: newPackage name = 'Collections-Sequenceable'.
	self assertEmpty: newPackage classes.
	self assertEmpty: newPackage traits.
	self assertEmpty: newPackage packages.
	self assert: newPackage package isNil.
	self assert: newPackage parent == Smalltalk globals
]
