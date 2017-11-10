"
A version reader
"
Class {
	#name : #MCVersionReader,
	#superclass : #MCReader,
	#instVars : [
		'package',
		'info',
		'definitions',
		'dependencies'
	],
	#category : #Monticello-Storing
}

{ #category : #reading }
MCVersionReader class >> file: fileName streamDo: aBlock [
	
	^ FileStream 
			readOnlyFileNamed: fileName 
			do: [:file | aBlock value: file]
]

{ #category : #'actions - file' }
MCVersionReader class >> loadVersionFile: fileName [
	| version |
	version := self versionFromFile: fileName.
	version workingCopy repositoryGroup addRepository:
		(MCDirectoryRepository new directory: fileName asFileReference).
	version load.

]

{ #category : #'actions - file' }
MCVersionReader class >> mergeVersionFile: fileName [
	(self versionFromFile: fileName) merge
]

{ #category : #reading }
MCVersionReader class >> on: s fileName: f [
	^ self on: s
]

{ #category : #'actions - file' }
MCVersionReader class >> openVersionFile: fileName [
	(self versionFromFile: fileName) open
]

{ #category : #reading }
MCVersionReader class >> versionFromFile: fileName [
	^ self file: fileName streamDo: [:stream | self versionFromStream: stream]
]

{ #category : #reading }
MCVersionReader class >> versionFromStream: aStream [
	^ (self on: aStream) version
]

{ #category : #reading }
MCVersionReader class >> versionInfoFromStream: aStream [
	^ (self on: aStream) info
]

{ #category : #accessing }
MCVersionReader >> basicVersion [
	^ MCVersion
		package: self package
		info: self info
		snapshot: [ self snapshot ]
		dependencies: self dependencies
]

{ #category : #accessing }
MCVersionReader >> definitions [
	definitions ifNil: [self loadDefinitions].
	^ definitions
]

{ #category : #accessing }
MCVersionReader >> dependencies [
	dependencies ifNil: [self loadDependencies].
	^ dependencies
]

{ #category : #accessing }
MCVersionReader >> info [
	info ifNil: [self loadVersionInfo].
	^ info
]

{ #category : #loading }
MCVersionReader >> loadDefinitions [
	self subclassResponsibility 
]

{ #category : #loading }
MCVersionReader >> loadDependencies [
	self subclassResponsibility 
]

{ #category : #loading }
MCVersionReader >> loadPackage [
	self subclassResponsibility 
]

{ #category : #loading }
MCVersionReader >> loadVersionInfo [
	self subclassResponsibility
]

{ #category : #accessing }
MCVersionReader >> package [
	package ifNil: [self loadPackage].
	^ package
]

{ #category : #accessing }
MCVersionReader >> snapshot [
	^ MCSnapshot fromDefinitions: self definitions
]

{ #category : #accessing }
MCVersionReader >> version [
	^ self basicVersion
]
