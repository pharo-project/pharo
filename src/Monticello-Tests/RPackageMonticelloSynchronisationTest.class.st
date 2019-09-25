"
SUnit tests for RPackage Monticello synchronization
"
Class {
	#name : #RPackageMonticelloSynchronisationTest,
	#superclass : #RPackageMCSynchronisationTest,
	#category : #'Monticello-Tests-RPackage'
}

{ #category : #'tests - operations on MCPackages' }
RPackageMonticelloSynchronisationTest >> testAddMCPackageCreatesAPackage [
	"test that when we create a MCPackage, a corresponding package is created"
	
	MCWorkingCopy forPackage: (MCPackage new name: #Zork).
	self assert: (self organizer includesPackageNamed: #Zork).
		
					  
]

{ #category : #'tests - operations on MCPackages' }
RPackageMonticelloSynchronisationTest >> testAddMCPackageForCategoryAlreadyExistingDoesNotCreateAPackage [
	"test that when we create a MCPackage and that a category of this name already exists, no package is created"
 
	|tmpPackage|
	testingEnvironment organization addCategory: 'Zork'.
	tmpPackage := self organizer packageNamed: #Zork.
	MCWorkingCopy forPackage: (MCPackage new name: #Zork).
	self assert: tmpPackage equals: (self organizer packageNamed: #Zork).
		
		
					  
]

{ #category : #'tests - operations on MCPackages' }
RPackageMonticelloSynchronisationTest >> testUnloadMCPackageRemovesRPackage [
	"test that when we remove a MC Package, the corresponding RPackage is removed from the organizer"

	MCWorkingCopy forPackage: (MCPackage new name: 'XXXXX').
	(self allManagers detect: [:each | each packageName = 'XXXXX']) unload.

	self deny: (self organizer includesPackageNamed: #XXXXX).
]

{ #category : #'tests - operations on MCPackages' }
RPackageMonticelloSynchronisationTest >> testUnregisterMCPackageKeepsRPackage [
	"test that when we remove a MC Package, the corresponding RPackage is removed from the organizer"
	
	MCWorkingCopy forPackage: (MCPackage new name: 'XXXXX').
	(self allManagers detect: [:each | each packageName = 'XXXXX']) unregister.
	
	self assert: (self organizer includesPackageNamed: #XXXXX).
]
