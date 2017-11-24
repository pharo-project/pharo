"
I'm a commit action to perform a package commit. 
"
Class {
	#name : #VSCommitPackageAction,
	#superclass : #VSCommitAction,
	#instVars : [
		'package'
	],
	#category : #'Versionner-Commit'
}

{ #category : #'instance creation' }
VSCommitPackageAction class >> new [
	self error: 'Use #package:'
]

{ #category : #'instance creation' }
VSCommitPackageAction class >> package: aPackage [
	^ self basicNew 
		initializePackage: aPackage;
		yourself
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> browseOn: aBrowser [
	aBrowser browsePackage: self
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> currentString [
	| workingCopy |
	
	workingCopy := self package workingCopy.
	^ [ workingCopy ancestry ancestorStringWithout: workingCopy packageName ]
		on: Error do: [ :e | '' ]
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> displayString [ 
	^ self package name
]

{ #category : #executing }
VSCommitPackageAction >> executeOn: aUnit [
	self 
		commitPackage: self packageName
		comment: aUnit comment
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> icon [
	^ Smalltalk ui icons iconNamed: #packageIcon
]

{ #category : #initialization }
VSCommitPackageAction >> initializePackage: aPackage [
	package := aPackage.
	self initialize
]

{ #category : #testing }
VSCommitPackageAction >> isPackage [ 
	^ true
]

{ #category : #accessing }
VSCommitPackageAction >> package [
	^ package
]

{ #category : #accessing }
VSCommitPackageAction >> packageName [
	^ self package name
]

{ #category : #accessing }
VSCommitPackageAction >> removeOn: aUnit [
	aUnit removePackageAction: self
]

{ #category : #accessing }
VSCommitPackageAction >> repository [
	^ self package repository
]

{ #category : #private }
VSCommitPackageAction >> workingCopy [ 
	^ self package workingCopy
]
