"
A PDPackageAnalyzerDiffTreeModel is to watch what are the difference between two analysis of dependencies.
"
Class {
	#name : 'DAPackageAnalyzerDiffTreePresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'treeDiff',
		'packageRelationGraphDiff'
	],
	#category : 'Tool-DependencyAnalyser-UI-Core',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Core'
}

{ #category : 'instance creation' }
DAPackageAnalyzerDiffTreePresenter class >> newWith: oldRelationGraph on: newRelationGraph [
	^ self new
		initializeWith: oldRelationGraph on: newRelationGraph;
		yourself
]

{ #category : 'specs' }
DAPackageAnalyzerDiffTreePresenter class >> title [

	^ 'Package Dependencies Analysis Diff'
]

{ #category : 'initialization' }
DAPackageAnalyzerDiffTreePresenter >> buildRoots [
	self treeDiff roots: packageRelationGraphDiff packagesDiffToDisplay.
	self treeDiff whenBuiltDo: [ treeDiff children: [ :item | DAPackageItemDiffNode new content: item ] ]
]

{ #category : 'specs' }
DAPackageAnalyzerDiffTreePresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: #treeDiff;
		yourself
]

{ #category : 'protocol' }
DAPackageAnalyzerDiffTreePresenter >> extent [
	^ 900@900
]

{ #category : 'initialization' }
DAPackageAnalyzerDiffTreePresenter >> initializePresenters [

	treeDiff := self newTree
]

{ #category : 'initialization' }
DAPackageAnalyzerDiffTreePresenter >> initializeWith: oldRelationGraph on: newRelationGraph [
	packageRelationGraphDiff := DAPackageRelationGraphDiff new
		oldRelationGraph: oldRelationGraph;
		newRelationGraph: newRelationGraph.
	packageRelationGraphDiff make.

	self buildRoots
]

{ #category : 'accessing' }
DAPackageAnalyzerDiffTreePresenter >> treeDiff [
	^ treeDiff
]
