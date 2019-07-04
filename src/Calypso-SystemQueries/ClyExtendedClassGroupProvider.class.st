"
I provide ""extensions"" class group. I add this group only when package has extensions.
"
Class {
	#name : #ClyExtendedClassGroupProvider,
	#superclass : #ClySingleClassGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #enumerating }
ClyExtendedClassGroupProvider >> createClassGroupFor: aClassQuery from: aPackageScope [
	^ClyClassGroup named: 'Extensions' priority: 0 on: aClassQuery
]

{ #category : #enumerating }
ClyExtendedClassGroupProvider >> createClassQueryFrom: aPackageScope [
	^ClyAllClassesQuery from: (aPackageScope asScope: ClyPackageExtensionScope)
]
