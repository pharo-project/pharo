"
I browse the selected package in the package list of DrTests.
"
Class {
	#name : 'DTPackagesBrowseCommand',
	#superclass : 'DTPackagesCommand',
	#category : 'DrTests-Commands',
	#package : 'DrTests',
	#tag : 'Commands'
}

{ #category : 'default' }
DTPackagesBrowseCommand class >> defaultName [

	^ 'Browse'
]

{ #category : 'executing' }
DTPackagesBrowseCommand >> execute [

	self context browseSelectedPackage
]
