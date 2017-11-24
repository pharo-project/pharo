"
SUnit tests for packages
"
Class {
	#name : #RGPackageTest,
	#superclass : #TestCase,
	#category : #'Ring-Tests-Containers'
}

{ #category : #testing }
RGPackageTest >> testAddingClass [
	| newPackage newClass |
	
	newPackage:= RGPackage named: #'Collections-Sequenceable'.
	newClass:= RGClassDefinition named: #OrderedCollection.
	newPackage addClass: newClass.
	
	self assert: (newPackage hasClasses).
	self assert: (newClass package == newPackage).
	self assert: ((newPackage classNamed: #OrderedCollection) == newClass)
]

{ #category : #testing }
RGPackageTest >> testAddingPackage [
	| newPackage childPackage |
	
	newPackage:= RGPackage named: #'Collections'.
	newPackage addPackageNamed: #'Collections-Sequenceable'.
		
	self assert: (newPackage includesPackageNamed: #'Collections-Sequenceable').
	self assert: (newPackage packages size = 1).
	
	childPackage:= newPackage packageNamed: #'Collections-Sequenceable'.
	self assert: (childPackage parent = newPackage parent).
	self assert: (childPackage package = newPackage).
]

{ #category : #testing }
RGPackageTest >> testNewPackage [
	| newPackage |
	
	newPackage:= RGPackage named: 'Collections-Sequenceable'.
	
	self assert: (newPackage isPackage).
	self assert: (newPackage name = 'Collections-Sequenceable').
	self assert: (newPackage classes isEmpty).
	self assert: (newPackage traits isEmpty).
	self assert: (newPackage packages isEmpty).
	self assert: (newPackage package isNil).
	self assert: (newPackage parent == Smalltalk globals).
]
