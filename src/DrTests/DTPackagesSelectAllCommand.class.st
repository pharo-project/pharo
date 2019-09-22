"
I select all packages in the package list of DrTests.
"
Class {
	#name : #DTPackagesSelectAllCommand,
	#superclass : #DTPackagesCommand,
	#category : #'DrTests-Commands'
}

{ #category : #hooks }
DTPackagesSelectAllCommand >> execute [
	self drTest selectAllPackageList
]

{ #category : #hooks }
DTPackagesSelectAllCommand >> initialize [
	super initialize.
	self name: 'Select all'
]
