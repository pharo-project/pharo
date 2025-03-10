"
Warning signaled when a dependency manually declared has no match in potential dependencies.
"
Class {
	#name : #DAPotentialOutDatedDependencyWarning,
	#superclass : #Warning,
	#instVars : [
		#package,
		#dependency
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #signaling }
DAPotentialOutDatedDependencyWarning class >> signalDependency: dependencyName on: aPackageName [
	^ self basicNew
		initializeWithDependency: dependencyName on: aPackageName;
		signal;
		yourself
]

{ #category : #comparing }
DAPotentialOutDatedDependencyWarning >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ (package == anObject package) and: [ dependency == anObject dependency ]
]

{ #category : #accessing }
DAPotentialOutDatedDependencyWarning >> dependency [
	^ dependency
]

{ #category : #comparing }
DAPotentialOutDatedDependencyWarning >> hash [
	^ package hash bitXor: dependency hash
]

{ #category : #initialization }
DAPotentialOutDatedDependencyWarning >> initializeWithDependency: aDependencyName on: aPackageName [
	dependency := aDependencyName.
	package := aPackageName
]

{ #category : #accessing }
DAPotentialOutDatedDependencyWarning >> messageText [
	^ messageText
		ifNil: [ package , ': ' , dependency , ' dependency declared in the package Manifest as manuallyResolvedDependencies not detected as a dependency!' ]
]

{ #category : #accessing }
DAPotentialOutDatedDependencyWarning >> package [
	^ package
]
