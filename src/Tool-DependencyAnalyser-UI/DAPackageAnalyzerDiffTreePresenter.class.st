"
A PDPackageAnalyzerDiffTreeModel is to watch what are the difference between two analysis of dependencies.
"
Class {
	#name : #DAPackageAnalyzerDiffTreePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'treeDiff',
		'packageRelationGraphDiff'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #specs }
DAPackageAnalyzerDiffTreePresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed newColumn: [ :col | col add: #treeDiff ] yourself
]

{ #category : #'instance creation' }
DAPackageAnalyzerDiffTreePresenter class >> newWith: oldRelationGraph on: newRelationGraph [
	^ self new
		initializeWith: oldRelationGraph on: newRelationGraph;
		yourself.
]

{ #category : #initialization }
DAPackageAnalyzerDiffTreePresenter >> buildRoots [
	self treeDiff roots: packageRelationGraphDiff packagesDiffToDisplay.
	self treeDiff whenBuiltDo: [ treeDiff rootNodeHolder: [ :item | DAPackageItemDiffNode new content: item ] ]
]

{ #category : #protocol }
DAPackageAnalyzerDiffTreePresenter >> extent [
	^ 900@900
]

{ #category : #initialization }
DAPackageAnalyzerDiffTreePresenter >> initializeWidgets [
	
	treeDiff := self newTree
		multiSelection: true;
		yourself.
]

{ #category : #initialization }
DAPackageAnalyzerDiffTreePresenter >> initializeWith: oldRelationGraph on: newRelationGraph [
	packageRelationGraphDiff := DAPackageRelationGraphDiff new
		oldRelationGraph: oldRelationGraph;
		newRelationGraph: newRelationGraph.
	packageRelationGraphDiff make.
	
	self buildRoots
	
	
]

{ #category : #'instance creation' }
DAPackageAnalyzerDiffTreePresenter >> open [	
	^ self openWithSpec
]

{ #category : #protocol }
DAPackageAnalyzerDiffTreePresenter >> title [
	^ 'Package Dependencies Analysis Diff'
	
]

{ #category : #accessing }
DAPackageAnalyzerDiffTreePresenter >> treeDiff [
	^ treeDiff
]
