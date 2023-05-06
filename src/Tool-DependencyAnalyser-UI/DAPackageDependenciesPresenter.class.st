"
UI merging SpecTreeModel cycles and browser
"
Class {
	#name : #DAPackageDependenciesPresenter,
	#superclass : #DAPackageAnalyzerPresenter,
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #examples }
DAPackageDependenciesPresenter class >> collections [
	^ (self onPackagesMatch: '*Collections*') open
]

{ #category : #'instance creation' }
DAPackageDependenciesPresenter class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(Smalltalk organization packages
				select: [ :package | match match: package packageName asString ]
				thenCollect: [ :package | package packageName ]) )
]

{ #category : #examples }
DAPackageDependenciesPresenter class >> system [
	^ (self onPackagesNamed: self systemPackages) open
]

{ #category : #initialization }
DAPackageDependenciesPresenter >> initializeWithRelationGraph: aRelationGraph [
	self model: (DAReverseTreePresenter onRelationGraph: aRelationGraph).
	"self buildandAddAllMorph "
]
