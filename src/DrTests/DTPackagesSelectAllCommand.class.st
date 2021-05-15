"
I select all packages in the package list of DrTests.
"
Class {
	#name : #DTPackagesSelectAllCommand,
	#superclass : #DTPackagesCommand,
	#category : #'DrTests-Commands'
}

{ #category : #defaults }
DTPackagesSelectAllCommand class >> defaultName [

	^ 'Select all'
]

{ #category : #hooks }
DTPackagesSelectAllCommand >> execute [

	self context selectAllInPackageList
]
