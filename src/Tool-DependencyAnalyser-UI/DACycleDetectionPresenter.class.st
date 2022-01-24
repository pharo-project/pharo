"
UI merging SpecTreeModel package dependencies and browser
"
Class {
	#name : #DACycleDetectionPresenter,
	#superclass : #DAPackageAnalyzerPresenter,
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #examples }
DACycleDetectionPresenter class >> collections [
	^ (self onPackagesMatch: '*Collections*')
		open
]

{ #category : #'instance creation' }
DACycleDetectionPresenter class >> onPackagesMatch: match [
	^ (self onPackagesNamed:	self systemPackages) 
		open
]

{ #category : #examples }
DACycleDetectionPresenter class >> system [
	^ (self new initializeWithFilter: self systemPackages) 
		open
]

{ #category : #'initialization - filter' }
DACycleDetectionPresenter >> initializeCycleDetectionTreeModelWith: aCollection [

	self initializeWithSystemCycles.
	self dependenciesPresenter filterWithPackages: aCollection
]

{ #category : #'initialization - filter' }
DACycleDetectionPresenter >> initializeWithFilter: aCollection [
	self initializeCycleDetectionTreeModelWith: aCollection.
	self buildandAddAllMorph
]

{ #category : #'initialization - union' }
DACycleDetectionPresenter >> initializeWithPackageNames: aCollection [
	self model: (DACycleDetectionTreePresenter  onPackagesNamed: aCollection).
	self buildandAddAllMorph 
]

{ #category : #'initialization - filter' }
DACycleDetectionPresenter >> initializeWithSystemCycles [
	self model: DACycleDetectionTreePresenter system.
]

{ #category : #update }
DACycleDetectionPresenter >> updateWithFilter: aCollectionOfPDPackages [
	self dependenciesPresenter filterWithPackages: aCollectionOfPDPackages.
]
