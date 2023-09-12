Class {
	#name : #MCOrganizationTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests-Base'
}

{ #category : #tests }
MCOrganizationTest >> testLoadAndUnload [

	| packageName |
	packageName := 'TestPackageToUnload'.
	self packageOrganizer ensurePackage: packageName.
	(MCOrganizationDefinition packageName: packageName) unload.
	self deny: (self packageOrganizer includesCategory: packageName)
]
