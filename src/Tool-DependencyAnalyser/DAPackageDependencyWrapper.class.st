"
Wrapper for dependent package (use in TreePresenter)
"
Class {
	#name : #DAPackageDependencyWrapper,
	#superclass : #Object,
	#instVars : [
		'daPackage'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #'instance creation' }
DAPackageDependencyWrapper class >> newWith: aPackage [
	^ self new initializeFrom: aPackage
]

{ #category : #comparing }
DAPackageDependencyWrapper >> <= other [
	^ self daPackage <= other daPackage
]

{ #category : #accessing }
DAPackageDependencyWrapper >> daPackage [
	^ daPackage
]

{ #category : #accessing }
DAPackageDependencyWrapper >> daPackage: anObject [
	daPackage := anObject
]

{ #category : #initialization }
DAPackageDependencyWrapper >> initializeFrom: aPackage [
	super initialize.
	self daPackage: aPackage.
]

{ #category : #accessing }
DAPackageDependencyWrapper >> packageName [
	^ daPackage packageName
]

{ #category : #printing }
DAPackageDependencyWrapper >> printOn: aStream [
	aStream nextPutAll: self packageName
]

{ #category : #accessing }
DAPackageDependencyWrapper >> rPackageSet [
	^ daPackage rPackageSet
]
