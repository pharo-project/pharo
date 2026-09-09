"
I inspect the selected package in the package list of DrTests.
"
Class {
	#name : 'DTPackagesInspectCommand',
	#superclass : 'DTPackagesCommand',
	#category : 'DrTests-Commands',
	#package : 'DrTests',
	#tag : 'Commands'
}

{ #category : 'default' }
DTPackagesInspectCommand class >> defaultName [

	^ 'Inspect'
]

{ #category : 'executing' }
DTPackagesInspectCommand >> execute [

	self context inspectSelectedPackage 
]
