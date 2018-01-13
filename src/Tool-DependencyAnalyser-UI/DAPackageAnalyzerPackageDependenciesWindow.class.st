"
UI merging SpecTreeModel cycles and browser
"
Class {
	#name : #DAPackageAnalyzerPackageDependenciesWindow,
	#superclass : #DAPackageAnalyzerWindow,
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #nautilus }
DAPackageAnalyzerPackageDependenciesWindow class >> browseDependencies: aBuilder [
	<contextMenu>
	<nautilusGlobalPackageMenu>
	(aBuilder item: #'Browse dependencies...')
		action: [ (self onPackagesNamed: (self selectedPackagesFrom: aBuilder)) open ];
		order: 1000.
]

{ #category : #examples }
DAPackageAnalyzerPackageDependenciesWindow class >> collections [
	^ (self onPackagesMatch: '*Collections*') open
]

{ #category : #'instance-creation' }
DAPackageAnalyzerPackageDependenciesWindow class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(RPackageOrganizer default packages
				select: [ :package | match match: package packageName asString ]
				thenCollect: [ :package | package packageName ]) ) 
]

{ #category : #examples }
DAPackageAnalyzerPackageDependenciesWindow class >> system [
	^ (self onPackagesNamed: self systemPackages) open 
]

{ #category : #initialization }
DAPackageAnalyzerPackageDependenciesWindow >> initializeWithPackageNames: aCollection [
	packageAnalyzerTreeModel := DAPackageAnalyzerTreeModel onPackagesNamed: aCollection.
	self buildandAddAllMorph 
]

{ #category : #initialization }
DAPackageAnalyzerPackageDependenciesWindow >> initializeWithRelationGraph: aRelationGraph [
	packageAnalyzerTreeModel := DAPackageAnalyzerReverseTreeModel onRelationGraph: aRelationGraph.
	self buildandAddAllMorph 
]
