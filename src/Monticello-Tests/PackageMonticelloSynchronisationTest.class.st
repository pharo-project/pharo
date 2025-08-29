"
SUnit tests for Package Monticello synchronization
"
Class {
	#name : 'PackageMonticelloSynchronisationTest',
	#superclass : 'PackageTestCase',
	#category : 'Monticello-Tests-Package',
	#package : 'Monticello-Tests',
	#tag : 'Package'
}

{ #category : 'accessing' }
PackageMonticelloSynchronisationTest >> allWorkingCopies [

	^ MCWorkingCopy allWorkingCopies
]

{ #category : 'tests - operations on MCPackages' }
PackageMonticelloSynchronisationTest >> testAddMCPackageCreatesAPackage [
	"test that when we create a MCPackage, a corresponding package is created"

	MCWorkingCopy ensureForPackageNamed: #Zork packageOrganizer: self organizer.
	self assert: (self organizer hasPackage: #Zork)
]

{ #category : 'tests - operations on MCPackages' }
PackageMonticelloSynchronisationTest >> testAddMCPackageForCategoryAlreadyExistingDoesNotCreateAPackage [
	"test that when we create a MCPackage and that a category of this name already exists, no package is created"

	| tmpPackage |
	self organizer ensurePackage: 'Zork'.
	tmpPackage := self organizer packageNamed: #Zork.
	MCWorkingCopy ensureForPackageNamed: #Zork packageOrganizer: self organizer.
	self assert: tmpPackage identicalTo: (self organizer packageNamed: #Zork)
]

{ #category : 'tests - operations on MCPackages' }
PackageMonticelloSynchronisationTest >> testUnloadMCPackageRemovesPackage [
	"test that when we remove a MC Package, the corresponding Package is removed from the organizer"

	MCWorkingCopy ensureForPackageNamed: 'XXXXX' packageOrganizer: self organizer.
	(self allWorkingCopies detect: [ :each | each packageName = 'XXXXX' ]) unload.

	self deny: (self organizer hasPackage: #XXXXX)
]

{ #category : 'tests - operations on MCPackages' }
PackageMonticelloSynchronisationTest >> testUnregisterMCPackageKeepsPackage [
	"test that when we remove a MC Package, the corresponding Package is removed from the organizer"

	MCWorkingCopy ensureForPackageNamed: 'XXXXX' packageOrganizer: self organizer.
	(self allWorkingCopies detect: [ :each | each packageName = 'XXXXX' ]) unregister.

	self assert: (self organizer hasPackage: #XXXXX)
]
