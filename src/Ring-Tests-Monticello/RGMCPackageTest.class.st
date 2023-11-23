Class {
	#name : 'RGMCPackageTest',
	#superclass : 'RGMCTest',
	#category : 'Ring-Tests-Monticello',
	#package : 'Ring-Tests-Monticello'
}

{ #category : 'tests' }
RGMCPackageTest >> testAsMCSnapshotHasOrganization [

	| newPackage snapshot |
	newPackage := RGPackage named: 'Collections-Sequenceable'.
	snapshot := newPackage asMCSnapshot.

	self assert: snapshot definitions size equals: 1.
	self assert: snapshot definitions anyOne isOrganizationDefinition.
	self assert: snapshot definitions anyOne packageName equals: 'Collections-Sequenceable'
]
