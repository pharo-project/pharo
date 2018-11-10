"
I'm the simplest possible checkout strategy: I will load into image all packages defined in repository regardless their previous status. 

This strategy is very dangerous since it can introduce in image package versions that were not there before. Or it may cause the packages/classes became incorrectly initialised. But some project can find this strategy very apeleable nevertheless.
"
Class {
	#name : #IceCheckoutAllPackages,
	#superclass : #IceCheckoutStrategy,
	#category : #'Iceberg-Core'
}

{ #category : #accessing }
IceCheckoutAllPackages class >> order [
	^ 20
]

{ #category : #checkout }
IceCheckoutAllPackages >> checkoutOn: anIceWorkingCopy [
	
	anIceWorkingCopy
		loadPackagesNamed: committish packageNames
		fromCommit: committish commit.
	self repository setHead: committish.
	anIceWorkingCopy referenceCommit: committish commit.
	anIceWorkingCopy markAllPackagesAsClean.
]
