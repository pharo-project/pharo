"
Dependencies
"
Class {
	#name : #MCVersionDependency,
	#superclass : #Object,
	#instVars : [
		'package',
		'versionInfo'
	],
	#category : #'Monticello-Versioning'
}

{ #category : #'instance creation' }
MCVersionDependency class >> package: aPackage info: aVersionInfo [
	^ self basicNew initializeWithPackage: aPackage info: aVersionInfo
]

{ #category : #comparing }
MCVersionDependency >> = other [
	^ other species = self species
		and: [other versionInfo = versionInfo
				and: [other package = package]]
]

{ #category : #comparing }
MCVersionDependency >> hash [
	^ versionInfo hash
]

{ #category : #initialization }
MCVersionDependency >> initializeWithPackage: aPackage info: aVersionInfo [
	package := aPackage.
	versionInfo := aVersionInfo
]

{ #category : #testing }
MCVersionDependency >> isCurrent [
	^ package hasWorkingCopy
		and: [self isFulfilled
			and: [package workingCopy modified not]]
]

{ #category : #testing }
MCVersionDependency >> isFulfilled [
	^package hasWorkingCopy
		and: [self isFulfilledBy: package workingCopy ancestry]
]

{ #category : #testing }
MCVersionDependency >> isFulfilledBy: anAncestry [
	^ anAncestry ancestors includes: versionInfo
]

{ #category : #testing }
MCVersionDependency >> isFulfilledByAncestors [
	^ package hasWorkingCopy
		and: [self isFulfilledByAncestorsOf: package workingCopy ancestry]
]

{ #category : #testing }
MCVersionDependency >> isFulfilledByAncestorsOf: anAncestry [
	^ anAncestry hasAncestor: versionInfo
]

{ #category : #testing }
MCVersionDependency >> isOlder [
	"Answer true if I represent an older version of a package that is loaded."
	^ package hasWorkingCopy
		and: [self isFulfilled not
			and: [ self isFulfilledByAncestors
				and: [package workingCopy modified not]]]
]

{ #category : #accessing }
MCVersionDependency >> package [
	^ package
]

{ #category : #accessing }
MCVersionDependency >> repositoryGroup [
	^ self package workingCopy repositoryGroup
]

{ #category : #resolving }
MCVersionDependency >> resolve [
	^ self repositoryGroup
		versionWithInfo: versionInfo
		ifNone: [ MCRepositoryGroup default versionWithInfo: versionInfo ifNone: []]
]

{ #category : #accessing }
MCVersionDependency >> versionInfo [
	^ versionInfo
]
