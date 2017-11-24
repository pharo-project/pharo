"
A PDPackageAnalyzerDiffTreeModel is to watch what are the difference between two analysis of dependencies.
"
Class {
	#name : #DAPackageAnalyzerDiffTreeModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'treeDiff',
		'packageRelationGraphDiff'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #specs }
DAPackageAnalyzerDiffTreeModel class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed newColumn: [ :col | col add: #treeDiff ] yourself
]

{ #category : #'instance creation' }
DAPackageAnalyzerDiffTreeModel class >> newWith: oldRelationGraph on: newRelationGraph [
	^ self new
		initializeWith: oldRelationGraph on: newRelationGraph;
		yourself.
]

{ #category : #initialization }
DAPackageAnalyzerDiffTreeModel >> buildRoots [
	self treeDiff roots: packageRelationGraphDiff packagesDiffToDisplay.
	self treeDiff whenBuiltDo: [ treeDiff rootNodeHolder: [ :item | DAPackageItemDiffNode new content: item ] ]
]

{ #category : #protocol }
DAPackageAnalyzerDiffTreeModel >> extent [
	^ 900@900
]

{ #category : #initialization }
DAPackageAnalyzerDiffTreeModel >> initializeWidgets [
	
	treeDiff := self newTree
		multiSelection: true;
		yourself.
]

{ #category : #initialization }
DAPackageAnalyzerDiffTreeModel >> initializeWith: oldRelationGraph on: newRelationGraph [
	packageRelationGraphDiff := DAPackageRelationGraphDiff new
		oldRelationGraph: oldRelationGraph;
		newRelationGraph: newRelationGraph.
	packageRelationGraphDiff make.
	
	self buildRoots
	
	
]

{ #category : #'instance creation' }
DAPackageAnalyzerDiffTreeModel >> open [	
	^ self openWithSpec
]

{ #category : #protocol }
DAPackageAnalyzerDiffTreeModel >> title [
	^ 'Package Dependencies Analysis Diff'
	
]

{ #category : #accessing }
DAPackageAnalyzerDiffTreeModel >> treeDiff [
	^ treeDiff
]
