"
A PDDependentPackageItemDiff is a object which represent a dependent package who added/modified/removed from an old analysis.
"
Class {
	#name : #DADependentPackageItemDiff,
	#superclass : #DAAbstractItemDiff,
	#instVars : [
		'parent'
	],
	#category : #'Tool-DependencyAnalyser-Diff'
}

{ #category : #testing }
DADependentPackageItemDiff >> hasChanged [
	^ self isNew or: [ self isRemoved ].
]

{ #category : #accessing }
DADependentPackageItemDiff >> parent [
	^ parent
]

{ #category : #accessing }
DADependentPackageItemDiff >> parent: anObject [
	parent := anObject
]

{ #category : #printing }
DADependentPackageItemDiff >> printOn: aStream [ 
	aStream 
		nextPutAll: content packageName;
		nextPutAll: ' Parent : ';
		nextPutAll: self parent packageName
]
