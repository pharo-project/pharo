"
A dictionary repository (in nemory)
"
Class {
	#name : #MCDictionaryRepository,
	#superclass : #MCRepository,
	#instVars : [
		'description',
		'dict'
	],
	#category : #'Monticello-Repositories'
}

{ #category : #comparing }
MCDictionaryRepository >> = other [
	^ self == other
]

{ #category : #accessing }
MCDictionaryRepository >> allVersionInfos [
	^ dict values collect: [:ea | ea info]
]

{ #category : #storing }
MCDictionaryRepository >> basicStoreVersion: aVersion [
	dict at: aVersion info put: aVersion
]

{ #category : #accessing }
MCDictionaryRepository >> closestAncestorVersionFor: anAncestry ifNone: errorBlock [
	^ anAncestry breadthFirstAncestors
		detect: [ :ea | self includesVersionWithInfo: ea ]
		ifFound: [ :info | self versionWithInfo: info ]
		ifNone: errorBlock
]

{ #category : #accessing }
MCDictionaryRepository >> description [

	^ description ifNil: ['cache']
]

{ #category : #accessing }
MCDictionaryRepository >> description: aString [

	description := aString 
]

{ #category : #accessing }
MCDictionaryRepository >> dictionary [

	^ dict
]

{ #category : #accessing }
MCDictionaryRepository >> dictionary: aDictionary [

	dict := aDictionary
]

{ #category : #comparing }
MCDictionaryRepository >> hash [

	^ self identityHash
]

{ #category : #interface }
MCDictionaryRepository >> includesVersionNamed: aString [
	^ dict anySatisfy: [:ea | ea info name = aString]
]

{ #category : #testing }
MCDictionaryRepository >> includesVersionWithInfo: aVersionInfo [
	^ dict includesKey: aVersionInfo
]

{ #category : #initialization }
MCDictionaryRepository >> initialize [

	super initialize.
	dict := Dictionary new.

]

{ #category : #accessing }
MCDictionaryRepository >> possiblyNewerVersionsOfAnyOf: versionNames [ 

	^#()
]

{ #category : #accessing }
MCDictionaryRepository >> sortedVersionInfos [
	| sorter |
	sorter := MCVersionSorter new.
	self allVersionInfos do: [:ea | sorter addVersionInfo: ea].
	^ sorter sortedVersionInfos

]

{ #category : #accessing }
MCDictionaryRepository >> versionFrom: aVersionReferenceString [
	^ self dictionary detect: [ :version | version info name = aVersionReferenceString ]
]

{ #category : #interface }
MCDictionaryRepository >> versionWithInfo: aVersionInfo ifAbsent: errorBlock [
	^ dict at: aVersionInfo ifAbsent: errorBlock
]
