"
UI merging SpecTreeModel package dependencies and browser
"
Class {
	#name : #DAPackageAnalyzerCycleDetectionWindow,
	#superclass : #DAPackageAnalyzerWindow,
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #examples }
DAPackageAnalyzerCycleDetectionWindow class >> collections [
	^ (self onPackagesMatch: '*Collections*')
		open
]

{ #category : #'instance-creation' }
DAPackageAnalyzerCycleDetectionWindow class >> onPackagesMatch: match [
	^ (self onPackagesNamed:	self systemPackages) 
		open
]

{ #category : #examples }
DAPackageAnalyzerCycleDetectionWindow class >> system [
	^ (self new initializeWithFilter: self systemPackages) 
		open
]

{ #category : #'initialization-filter' }
DAPackageAnalyzerCycleDetectionWindow >> initializeCycleDetectionTreeModelWith: aCollection [

	self initializeWithSystemCycles.
	packageAnalyzerTreeModel filterWithPackages: aCollection
]

{ #category : #'initialization-filter' }
DAPackageAnalyzerCycleDetectionWindow >> initializeWithFilter: aCollection [
	self initializeCycleDetectionTreeModelWith: aCollection.
	self buildandAddAllMorph
]

{ #category : #'initialization-union' }
DAPackageAnalyzerCycleDetectionWindow >> initializeWithPackageNames: aCollection [
	packageAnalyzerTreeModel  := DAPackageCycleDetectionTreeModel  onPackagesNamed: aCollection.
	self buildandAddAllMorph 
]

{ #category : #'initialization-filter' }
DAPackageAnalyzerCycleDetectionWindow >> initializeWithSystemCycles [
	packageAnalyzerTreeModel := DAPackageCycleDetectionTreeModel system.
]

{ #category : #update }
DAPackageAnalyzerCycleDetectionWindow >> updateWithFilter: aCollectionOfPDPackages [
	self packageAnalyzerTreeModel filterWithPackages: aCollectionOfPDPackages.
]
