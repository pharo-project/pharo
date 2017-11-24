"
Warning signaled when a dependency manually declared has no match in potential dependencies.
"
Class {
	#name : #PotentialOutDatedDependencyWarning,
	#superclass : #Warning,
	#instVars : [
		'package',
		'dependency'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #signaling }
PotentialOutDatedDependencyWarning class >> signalDependency: dependencyName on: aPackageName [
	^ self basicNew 
		initializeWithDependency: dependencyName on: aPackageName;
		signal;
		yourself
]

{ #category : #comparing }
PotentialOutDatedDependencyWarning >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ (package == anObject package) and: [ dependency == anObject dependency ]
]

{ #category : #accessing }
PotentialOutDatedDependencyWarning >> dependency [
	^ dependency
]

{ #category : #comparing }
PotentialOutDatedDependencyWarning >> hash [
	^ package hash bitXor: dependency hash
]

{ #category : #initialization }
PotentialOutDatedDependencyWarning >> initializeWithDependency: aDependencyName on: aPackageName [ 
	dependency := aDependencyName.
	package := aPackageName
]

{ #category : #accessing }
PotentialOutDatedDependencyWarning >> messageText [
	^ messageText 
		ifNil: [ package , ': ' , dependency , ' dependency declared in the package Manifest as manuallyResolvedDependencies not detected as a dependency!' ]
]

{ #category : #accessing }
PotentialOutDatedDependencyWarning >> package [
	^ package
]
