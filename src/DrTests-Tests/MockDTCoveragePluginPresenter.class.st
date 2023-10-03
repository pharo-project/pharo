Class {
	#name : 'MockDTCoveragePluginPresenter',
	#superclass : 'DTCoveragePluginPresenter',
	#category : 'DrTests-Tests',
	#package : 'DrTests-Tests'
}

{ #category : 'accessing - attributes' }
MockDTCoveragePluginPresenter >> coveredNodes [
	^ coveredNodes 
]

{ #category : 'accessing - attributes' }
MockDTCoveragePluginPresenter >> uncoveredNodes [
	^ uncoveredNodes 
]

{ #category : 'initialization' }
MockDTCoveragePluginPresenter >> updatePresenter [
	"I rewrite this method by commenting, so this class  cannot be initialized and run other methods that are unnecessary to perform the tests."

	"super updatePresenter.
	self updatePackagesList.
	self updateResultViewsDropList."

	
]

{ #category : 'accessing - attributes' }
MockDTCoveragePluginPresenter >> yellowNodes [
	^ yellowNodes 
]
