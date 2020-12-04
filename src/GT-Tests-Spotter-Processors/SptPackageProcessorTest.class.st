Class {
	#name : #SptPackageProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptPackageProcessorTest >> processor [

	^ SptPackageProcessor new
]

{ #category : #tests }
SptPackageProcessorTest >> testFindThisPackage [

	self runForText: 'gt-tests-spotter-processors'.
	self assertResultsIncludes: self class package
]

{ #category : #tests }
SptPackageProcessorTest >> testFindThisPackageAfterAddingText [

	self runForText: 'gt-tests-spo'.
	self runForText: 'gt-tests-spotter-processors'.

	self assertResultsIncludes: self class package
	
	
]

{ #category : #tests }
SptPackageProcessorTest >> testGTSpotterNewIsRemovedAfterAddingText [

	self runForText: 'gt-spotter'.
	self rerunWithText: 'gt-spotter-new-te'.
	
	self denyResultsIncludes: SptClassProcessor package
]

{ #category : #tests }
SptPackageProcessorTest >> testPopularStringDoesReturnOnlyTenResults [

	self runForText: 'test'.
	self assertQuantityOfResults: 25.
]
