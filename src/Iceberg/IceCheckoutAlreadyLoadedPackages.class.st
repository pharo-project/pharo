"
I'm a strategy thet will load into image JUST the packages that were already loaded on it. 
Loading same-named packages of those already in image is probably the best strategy possible before an installation following a project description.

This strategy is still dangerous bacause packages installed may be incorrectly initialisated. Or a refactor could have split a package and now half of the data are not there anymore. But still... is the best possible approach when you do not have a propper project description.

"
Class {
	#name : #IceCheckoutAlreadyLoadedPackages,
	#superclass : #IceCheckoutStrategy,
	#category : #'Iceberg-Core'
}

{ #category : #accessing }
IceCheckoutAlreadyLoadedPackages class >> order [
	^ 10
]

{ #category : #checkout }
IceCheckoutAlreadyLoadedPackages >> checkoutOn: anIceWorkingCopy [ 
	
	anIceWorkingCopy loadChangesInWorkingCopy: self diff.
	self repository setHead: committish.
	anIceWorkingCopy referenceCommit: committish commit.
	anIceWorkingCopy markAllPackagesAsClean.
]
