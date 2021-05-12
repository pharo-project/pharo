"
I deselect all packages in the package list of DrTests.
"
Class {
	#name : #DTPackagesSelectNoneCommand,
	#superclass : #DTPackagesCommand,
	#category : #'DrTests-Commands'
}

{ #category : #defaults }
DTPackagesSelectNoneCommand class >> defaultName [

	^ 'Select none'
]

{ #category : #executing }
DTPackagesSelectNoneCommand >> execute [

	self context selectNoneInPackageList
]
