Class {
	#name : 'MCOrganizationTest',
	#superclass : 'MCTestCase',
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
MCOrganizationTest >> testLoadAndUnload [

	| packageName |
	packageName := 'TestPackageToUnload'.
	self packageOrganizer ensurePackage: packageName.
	(MCOrganizationDefinition packageName: packageName) unload.
	self deny: (self packageOrganizer hasPackage: packageName)
]
