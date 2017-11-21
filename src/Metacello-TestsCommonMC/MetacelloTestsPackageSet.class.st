"
This class is used to speed up Metacello tests.

"
Class {
	#name : #MetacelloTestsPackageSet,
	#superclass : #RPackageSet,
	#category : #Metacello-TestsCommonMC
}

{ #category : #listing }
MetacelloTestsPackageSet >> extensionMethods [
	"Test packages don't extend classes and skipping this operation is a significant performance gain for running tests"
	^ #()
]

{ #category : #listing }
MetacelloTestsPackageSet >> overriddenMethods [
	"Test packages don't override methods and skipping this operation is a significant performance gain for running tests"
	^ #()
]
