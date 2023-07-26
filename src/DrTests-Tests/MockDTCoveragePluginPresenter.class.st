Class {
	#name : #MockDTCoveragePluginPresenter,
	#superclass : #DTCoveragePluginPresenter,
	#category : #'DrTests-Tests'
}

{ #category : #'accessing - attributes' }
MockDTCoveragePluginPresenter >> coveredNodes [
	^ coveredNodes 
]

{ #category : #'accessing - attributes' }
MockDTCoveragePluginPresenter >> uncoveredNodes [
	^ uncoveredNodes 
]

{ #category : #initialization }
MockDTCoveragePluginPresenter >> updatePresenter [

	"super updatePresenter."

	"self updatePackagesList."
	"self updateResultViewsDropList."
]

{ #category : #'accessing - attributes' }
MockDTCoveragePluginPresenter >> yellowNodes [
	^ yellowNodes 
]
