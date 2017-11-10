"
A PDAbstractItemDiff is a object which represent a package who added/modified/removed from an old analysis.
"
Class {
	#name : #DAPackageItemDiff,
	#superclass : #DAAbstractItemDiff,
	#category : #Tool-DependencyAnalyser-Diff
}

{ #category : #accessing }
DAPackageItemDiff >> dependencies [
	 ^ self content dependentPackages 
]

{ #category : #testing }
DAPackageItemDiff >> hasChanged [
	^ diff dependentPackagesDiff
		detect: [ :each | each parent = self content and: [ each hasChanged ] ]
		ifFound: [ true ]
		ifNone: [ false ]
]

{ #category : #printing }
DAPackageItemDiff >> printOn: aStream [
	aStream 
		nextPutAll: content packageName.
]
