Class {
	#name : #SptClassProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptClassProcessorTest >> processor [

	^ SptClassProcessor new
]

{ #category : #tests }
SptClassProcessorTest >> testFindGTClassProcessorTest [

	self runForText: 'sptclassprocessortest'.
	self assertResultsIncludes: SptClassProcessorTest
]

{ #category : #tests }
SptClassProcessorTest >> testFindGTClassProcessorTestAfterAddingTest [

	self runForText: 'sptclass'.
	self rerunWithText: 'sptclassprocess'.
	
	self assertResultsIncludes: SptClassProcessorTest 
]

{ #category : #tests }
SptClassProcessorTest >> testFindGTClassProcessorTestWithPartialText [

	self runForText: 'sptclass'.
	self assertResultsIncludes: SptClassProcessorTest
]

{ #category : #tests }
SptClassProcessorTest >> testGTClassProcessorIsRemovedAfterUpdatingText [

	self runForText: 'gtclass'.
	self rerunWithText: 'gtclassprocesst'.
	
	self denyResultsIncludes: SptClassProcessor
]

{ #category : #tests }
SptClassProcessorTest >> testPopularStringDoesReturnOnlyTenResults [

	self runForText: 'test'.
	self assertQuantityOfResults: 25.
]
