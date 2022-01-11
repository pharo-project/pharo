"
UI merging SpecTreeModel cycles and browser
"
Class {
	#name : #DAPackageAnalyzerPackageDependenciesPresenter,
	#superclass : #DAPackageAnalyzerWindow,
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #examples }
DAPackageAnalyzerPackageDependenciesPresenter class >> collections [
	^ (self onPackagesMatch: '*Collections*') open
]

{ #category : #'instance creation' }
DAPackageAnalyzerPackageDependenciesPresenter class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(RPackageOrganizer default packages
				select: [ :package | match match: package packageName asString ]
				thenCollect: [ :package | package packageName ]) ) 
]

{ #category : #examples }
DAPackageAnalyzerPackageDependenciesPresenter class >> system [
	^ (self onPackagesNamed: self systemPackages) open 
]

{ #category : #initialization }
DAPackageAnalyzerPackageDependenciesPresenter >> initializeWithPackageNames: aCollection [
	packageAnalyzerTreeModel := DAPackageAnalyzerTreePresenter onPackagesNamed: aCollection.
	self buildandAddAllMorph
]

{ #category : #initialization }
DAPackageAnalyzerPackageDependenciesPresenter >> initializeWithRelationGraph: aRelationGraph [
	packageAnalyzerTreeModel := DAPackageAnalyzerReverseTreePresenter onRelationGraph: aRelationGraph.
	self buildandAddAllMorph 
]
