"
Other wrapper for dependent package (use in TreePresenter). Useful in reverse analysis.
"
Class {
	#name : #DADependentPackageWrapper,
	#superclass : #Object,
	#instVars : [
		'pdPackage',
		'relationGraph'
	],
	#category : 'Tool-DependencyAnalyser-Core'
}

{ #category : #'instance creation' }
DADependentPackageWrapper class >> newWith: aPackage on: relationGraph [
	^ self new initializeFrom: aPackage on: relationGraph
]

{ #category : #comparing }
DADependentPackageWrapper >> <= other [
	^ self daPackage <= other daPackage
]

{ #category : #accessing }
DADependentPackageWrapper >> daPackage [
	^ pdPackage
]

{ #category : #accessing }
DADependentPackageWrapper >> daPackage: anObject [
	pdPackage := anObject
]

{ #category : #accessing }
DADependentPackageWrapper >> dependenciesSize [
	| usersOf |
	usersOf := self usersOfPackage.
	^ usersOf
			 inject: 0
			 into: [ :sum :each | sum + (each dependenciesSizeTo: self daPackage) ]
]

{ #category : #accessing }
DADependentPackageWrapper >> extensionDependenciesSize [
	| usersOf |
	usersOf := self usersOfPackage.
	^ usersOf 
			inject: 0 
			into: [ :sum :each | sum + (each extensionDependenciesSizeTo: self daPackage) ]
]

{ #category : #initialization }
DADependentPackageWrapper >> initializeFrom: aPackage on: aRelationGraph [
	pdPackage := aPackage.
	relationGraph :=aRelationGraph.
]

{ #category : #accessing }
DADependentPackageWrapper >> packageName [
	^ self daPackage packageName
]

{ #category : #printing }
DADependentPackageWrapper >> printOn: aStream [
	| numberOfExtension |
	numberOfExtension := self extensionDependenciesSize.
	aStream
		nextPutAll: self packageName;
		nextPutAll: ' <-- Users : ';
		nextPutAll: self usersOfPackageSize asString;
		nextPutAll: ' package(s) | ';
		nextPutAll: 'Dependencies : ';
		nextPutAll: ((self dependenciesSize) - numberOfExtension) asString;
		nextPutAll: ' (+ ';
		nextPutAll: numberOfExtension asString;
		nextPutAll: ' extension dependencies ';
		nextPutAll: ')'.
]

{ #category : #accessing }
DADependentPackageWrapper >> rPackageSet [
	^ pdPackage rPackageSet
]

{ #category : #accessing }
DADependentPackageWrapper >> relationGraph [
	^ relationGraph
]

{ #category : #accessing }
DADependentPackageWrapper >> relationGraph: anObject [
	relationGraph := anObject
]

{ #category : #accessing }
DADependentPackageWrapper >> usersOfPackage [
	^ relationGraph usersOf: pdPackage
]

{ #category : #accessing }
DADependentPackageWrapper >> usersOfPackageSize [
	^ self usersOfPackage size
]
