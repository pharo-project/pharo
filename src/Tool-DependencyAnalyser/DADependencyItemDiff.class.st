"
A PDDependencyItemDiff is a object which represent a dependent package who added/removed from an old analysis.
"
Class {
	#name : #DADependencyItemDiff,
	#superclass : #DAAbstractItemDiff,
	#category : #Tool-DependencyAnalyser-Diff
}

{ #category : #testing }
DADependencyItemDiff >> hasChanged [
	^ self 
]

{ #category : #printing }
DADependencyItemDiff >> printOn: aStream [
	aStream 
		nextPutAll: self content asString.
]
