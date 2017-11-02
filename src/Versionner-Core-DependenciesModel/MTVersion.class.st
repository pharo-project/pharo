"
A MTVersion describes a specific version of a dependency.

Instance Variables
	- author : the version author
	- versionString : a version String that can be used to load the described version (e.g ConfigurationOfXXX project version: versionString)
	- description : the version description 
	- timestamp : the commit time of this version
	- project : a reference to the described project
"
Class {
	#name : #MTVersion,
	#superclass : #Object,
	#instVars : [
		'name',
		'author',
		'versionString',
		'description',
		'timestamp',
		'project'
	],
	#category : 'Versionner-Core-DependenciesModel'
}

{ #category : #'instance creation' }
MTVersion class >> fromVersionName: aVersionName [
	"Create a new version from a versionName (a file reference)."

	| version names |
	names := MetacelloCommonMCSpecLoader nameComponentsFrom: aVersionName.
	version := self new.
	version name: names first.
	version author: (names at: 2).
	version versionString: (names at: 3).
	^ (version versionString = 0) ifTrue: [ nil ] ifFalse: [ version ]

]

{ #category : #'instance creation' }
MTVersion class >> fromVersionString: aVersionString [
	"Create a new version from a versionString."

	| version |
	version := self new.
	version versionString: aVersionString .
	^ version

]

{ #category : #comparing }
MTVersion >> = other [
	(self name = other name) ifFalse: [ ^false ].
	(self author = other author) ifFalse: [ ^false ].
	(self versionString = other versionString) ifFalse: [ ^false ].
	(self description = other description) ifFalse: [ ^false ].
	(self timestamp = other timestamp) ifFalse: [ ^false ].
	(self project = other project) ifFalse: [ ^false ].
	^true.
]

{ #category : #visiting }
MTVersion >> acceptVisitor: anMTProjectVisitor [
	anMTProjectVisitor visitVersion: self.
]

{ #category : #accessing }
MTVersion >> author [
	^ author
]

{ #category : #accessing }
MTVersion >> author: anObject [
	author := anObject
]

{ #category : #accessing }
MTVersion >> description [
	^ description
]

{ #category : #accessing }
MTVersion >> description: anObject [
	description := anObject
]

{ #category : #accessing }
MTVersion >> mcVersion [
	"Get the MetacelloMCVersion object representing this version"

	| configurationClass |
	configurationClass := project configurationClass.
	configurationClass project version: versionString. 
]

{ #category : #accessing }
MTVersion >> name [
	^name.
]

{ #category : #accessing }
MTVersion >> name: aName [ 
	name := aName.
]

{ #category : #printing }
MTVersion >> printOn: aStream [
	"Pretty print for a version"
	aStream "nextPutAll: name ;
			nextPut: $- ;
			nextPutAll: author ;
			nextPut: $. ;"
			nextPutAll: versionString asString .
]

{ #category : #accessing }
MTVersion >> project [
	"Get the project this version is refering to. May be a project or a required project"
	^ project
]

{ #category : #accessing }
MTVersion >> timestamp [
	^ timestamp
]

{ #category : #accessing }
MTVersion >> timestamp: anObject [
	timestamp := anObject
]

{ #category : #accessing }
MTVersion >> versionString [
	^ versionString
]

{ #category : #accessing }
MTVersion >> versionString: aVersionString [
	versionString := aVersionString.
	name := aVersionString.
]
