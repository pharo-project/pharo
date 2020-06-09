Class {
	#name : #SptUnifiedProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptUnifiedProcessorTest >> processor [

	^ SptUnifiedProcessor new
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindGTClassProcessorTest [

	self runForText: 'sptunifiedprocessortest'.
	self assertResultsIncludes: SptUnifiedProcessorTest
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindGTClassProcessorTestAfterAddingTest [

	self runForText: 'sptun'.
	self rerunWithText: 'sptunifiedprocess'.
	
	self assertResultsIncludes: self class
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindGTClassProcessorTestWithPartialText [

	self runForText: 'sptunifi'.
	self assertResultsIncludes: self class
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisMethod [

	self runForText: 'testFindThisMethod'.
	self assertResultsIncludes: thisContext method
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisMethodAfterAddingText [

	self runForText: 'test'.
	self runForText: 'testFindThisMethod'.
	
	self assertResultsIncludes: thisContext method
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisMethodWithFullClass [

	self runForText: 'SptUnifiedProcessorTest >> #testFindThisMethod'.
	self assertResultsIncludes: thisContext method
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisMethodWithFullClassDoesNotIncludeOtherClassMethod [

	self runForText: 'SptUnifiedProcessorTest >> #testFindThisMethod'.
	self denyResultsIncludes: SptImplementorsProcessorTest >> #testFindThisMethod
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisMethodWithFullClassIsTheCorrectclass [

	self runForText: 'SptUnifiedProcessorTest >> #testFindThisMethod'.
	self
		assert:
			((candidates results collect: [:each | each content])
				allSatisfy: [ :aMethod | aMethod methodClass = self class ])
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisPackage [

	self runForText: 'gt-tests-spotter-processors'.
	self assertResultsIncludes: SptUnifiedProcessorTest package
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisPackageAfterAddingText [

	self runForText: 'gt-tests-spo'.
	self runForText: 'gt-tests-spotter-processors'.

	self assertResultsIncludes: SptUnifiedProcessorTest package
	
	
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisPackageWithCategory [

	self runForText: '#Packages gt-tests-spotter-processors'.
	self assertResultsIncludes: SptUnifiedProcessorTest package
]

{ #category : #tests }
SptUnifiedProcessorTest >> testFindThisPackageWithOtherCategory [

	self runForText: '#Classes gt-spotter-new-tests'.
	self denyResultsIncludes: self class package
]

{ #category : #tests }
SptUnifiedProcessorTest >> testGTClassProcessorIsRemovedAfterUpdatingText [

	self runForText: 'gtclass'.
	self rerunWithText: 'gtclassprocesst'.
	
	self denyResultsIncludes: SptClassProcessor
]

{ #category : #tests }
SptUnifiedProcessorTest >> testGTSpotterNewIsRemovedAfterAddingText [

	self runForText: 'gt-spotter'.
	self rerunWithText: 'gt-spotter-new-te'.
	
	self denyResultsIncludes: SptClassProcessor package
]

{ #category : #tests }
SptUnifiedProcessorTest >> testLookingForDiskStoreWithSpaceReturnsDiskStore [

	self runForText: 'Disk Store'.
	self assertResultsIncludes: DiskStore
]

{ #category : #tests }
SptUnifiedProcessorTest >> testLookingForStoreDiskLowercaseReturnsThisMethod [

	self runForText: 'store disk'.
	self assertResultsIncludes: SptUnifiedProcessorTest >> #testLookingForStoreDiskLowercaseReturnsThisMethod
]

{ #category : #tests }
SptUnifiedProcessorTest >> testLookingForStoreDiskReturnsDiskStore [

	self runForText: 'StoreDisk'.
	self assertResultsIncludes: DiskStore
]

{ #category : #tests }
SptUnifiedProcessorTest >> testLookingForStoreDiskWithSpaceReturnsDiskStore [

	self runForText: 'Store Disk'.
	self assertResultsIncludes: DiskStore
]

{ #category : #tests }
SptUnifiedProcessorTest >> testLookingForStoreLowercaseReturnsDiskStore [

	self runForText: 'store disk'.
	self assertResultsIncludes: DiskStore
]

{ #category : #tests }
SptUnifiedProcessorTest >> testPopularStringDoesReturnOnlyTwentyResults [

	self runForText: 'test'.
	self assertQuantityOfResults: 25.
]

{ #category : #tests }
SptUnifiedProcessorTest >> testPopularStringDoesReturnOnlyTwentyResultsForPackage [

	self runForText: '-Test'.
	self assertQuantityOfResults: 25.
]

{ #category : #tests }
SptUnifiedProcessorTest >> testPopularStringDoesReturnOnlyTwentyResultsWithCapital [

	self runForText: 'Test'.
	self assertQuantityOfResults: 25.
]
