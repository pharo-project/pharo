"
A Metacello repository
"
Class {
	#name : 'MCRepository',
	#superclass : 'Object',
	#classVars : [
		'Settings'
	],
	#category : 'Monticello-Repositories',
	#package : 'Monticello',
	#tag : 'Repositories'
}

{ #category : 'accessing' }
MCRepository class >> allConcreteSubclasses [
	^ self withAllSubclasses reject: [:ea | ea isAbstract]
]

{ #category : 'accessing' }
MCRepository class >> description [
	^ nil
]

{ #category : 'instance creation' }
MCRepository class >> fromUrl: aUrlOrString [ 
	| url |
	url := aUrlOrString asUrl.

	^ self allSubclasses 
		detect: [ :each | 
			(each class includesSelector: #urlSchemes)
			and: [ each urlSchemes includes: url scheme ] ]
		ifFound: [ :repositoryClass | repositoryClass basicFromUrl: url ]
		ifNone: [ MCRepositoryError signal: 'Cannot instantiate a repository from the url: ', url asString ]
]

{ #category : 'testing' }
MCRepository class >> isAbstract [
	^ self description isNil
]

{ #category : 'accessing' }
MCRepository class >> urlSchemes [
	"Returns the URL schemes used to describe this repository"
	^ #()
]

{ #category : 'comparing' }
MCRepository >> = other [
	^ other species = self species and: [other description = self description]
]

{ #category : 'adding' }
MCRepository >> addTo: aRepository [
	^ aRepository addBasicRepository: self
]

{ #category : 'storing' }
MCRepository >> basicStoreVersion: aVersion [
	self subclassResponsibility
]

{ #category : 'accessing' }
MCRepository >> description [
	^ self class name
]

{ #category : 'displaying' }
MCRepository >> displayStringOn: aStream [
	aStream nextPutAll: self description
]

{ #category : 'error handling' }
MCRepository >> error: aString [ 
	^ MCRepositoryError new signal: aString
]

{ #category : 'comparing' }
MCRepository >> hash [
	^ self description hash
]

{ #category : 'interface' }
MCRepository >> includesVersionNamed: aString [
	self subclassResponsibility
]

{ #category : 'testing' }
MCRepository >> isValid [
	^true
]

{ #category : 'accessing' }
MCRepository >> packageNamed: aName [

	^ self packageNamed: aName ifAbsent: [ NotFound signal: aName ]
]

{ #category : 'accessing' }
MCRepository >> packageNamed: aName ifAbsent: aBlock [

	^ self
		packageNamed: aName
		ifPresent: [ :found | found ]
		ifAbsent: aBlock
]

{ #category : 'accessing' }
MCRepository >> packageNamed: aName ifPresent: aBlock [

	^ self
		  packageNamed: aName
		  ifPresent: aBlock
		  ifAbsent: [ NotFound signal: aName ]
]

{ #category : 'accessing' }
MCRepository >> packageNamed: aName ifPresent: presentBlock ifAbsent: absentBlock [

	^ absentBlock value
]

{ #category : 'printing' }
MCRepository >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self description;
		nextPut: $).
]

{ #category : 'storing' }
MCRepository >> storeVersion: aVersion [

	self basicStoreVersion: aVersion
]

{ #category : 'interface' }
MCRepository >> versionWithInfo: aVersionInfo [
	^ self versionWithInfo: aVersionInfo ifAbsent: [nil]
]

{ #category : 'interface' }
MCRepository >> versionWithInfo: aVersionInfo ifAbsent: aBlock [
	self subclassResponsibility 
]
