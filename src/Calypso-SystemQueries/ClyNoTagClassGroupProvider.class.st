"
I provide ""Uncategorized"" class group when there are class tags in the package but in same time there are classes without tags.

When package has no tags at all I do not provide any group.
"
Class {
	#name : #ClyNoTagClassGroupProvider,
	#superclass : #ClySingleClassGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClyNoTagClassGroupProvider >> createClassGroupFor: aClassQuery from: aPackageScope [
	^ClyNoTagClassGroup named: 'Uncategorized' priority: 100 on: aClassQuery
]

{ #category : #'building groups' }
ClyNoTagClassGroupProvider >> createClassQueryFrom: aPackageScope [
	^ClyRestUntaggedClassesQuery from: aPackageScope
]
