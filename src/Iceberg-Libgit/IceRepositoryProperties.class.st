"
I'm a class to keep the filetree properties file (present in all repository). 
I can answer concrete things about repositories, e.g. : 

- which is my file format (#cypress, #tonel, etc.)
- what are my reader/write filetree files
- ...
"
Class {
	#name : #IceRepositoryProperties,
	#superclass : #Object,
	#instVars : [
		'properties',
		'repository',
		'commitish'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #'instance creation' }
IceRepositoryProperties class >> fromDictionary: aDictionary commitish: aCommitish [
	^ self new
		repository: aCommitish repository;
		commitish: aCommitish;
		properties: aDictionary;
		ensureDefaultProperties;
		yourself
]

{ #category : #'instance creation' }
IceRepositoryProperties class >> fromFileReference: aFileReference commitish: aCommitish [

	aFileReference exists
		ifFalse: [ ^ IceUnbornRepositoryProperties fromDictionary: Dictionary new commitish: aCommitish ].
	^ self fromDictionary: (self readPropertiesFrom: aFileReference) commitish: aCommitish
]

{ #category : #'instance creation' }
IceRepositoryProperties class >> fromFileReferenceDirectory: aDirectory commitish: aCommitish [

	^ self fromFileReference: aDirectory / self propertiesFileName  commitish: aCommitish
]

{ #category : #accessing }
IceRepositoryProperties class >> propertiesFileName [
	^ '.properties'
]

{ #category : #'instance creation' }
IceRepositoryProperties class >> readPropertiesFrom: aFileReference [
	self flag: #clean.	"When Pharo 6.1 will not be supported anymore we can directly use: `ref readStreamDo: [ :stream | STON fromStream: stream ]` but currently FileReference>>readStream return a ZnBufferedStream in Pharo 61 and a ZnCharacterReadStream decoding utf8 in Pharo 7."
	^ aFileReference
		binaryReadStreamDo: [ :stream | STON fromStream: (ZnCharacterReadStream on: stream encoding: 'utf8') ]
]

{ #category : #comparing }
IceRepositoryProperties >> = anotherObject [

	^ self species = anotherObject species and: [ properties = anotherObject properties ]
]

{ #category : #writing }
IceRepositoryProperties >> addToChangeBuilder: anIceChangeImporter [ 

	anIceChangeImporter
		addFileNamed: self propertiesFilePath segments last
		path: self propertiesFilePath
		contents: self contentsString
]

{ #category : #converting }
IceRepositoryProperties >> asBornProperties [
	
	^ self
]

{ #category : #accessing }
IceRepositoryProperties >> at: aKey [

	^ self properties at: aKey
]

{ #category : #accessing }
IceRepositoryProperties >> at: aKey put: aValue [

	self properties at: aKey put: aValue
]

{ #category : #accessing }
IceRepositoryProperties >> commitish [
	^ commitish
]

{ #category : #accessing }
IceRepositoryProperties >> commitish: anObject [

1haltIf: [ anObject isNil ].
	commitish := anObject
]

{ #category : #accessing }
IceRepositoryProperties >> contentsString [
	
	^ ByteArray streamContents: [ :stream |
		(STONWriter on: (ZnCharacterWriteStream on: stream)) 
				prettyPrint: true;
				newLine: OSPlatform current lineEnding;
				nextPut: self properties ]
]

{ #category : #'initialization-release' }
IceRepositoryProperties >> ensureDefaultProperties [
	
	properties at: #format ifAbsentPut: [ 
		self writerClass id
	]
]

{ #category : #accessing }
IceRepositoryProperties >> fileFormat [
	^ self writerClass
]

{ #category : #accessing }
IceRepositoryProperties >> fileFormat: aClass [
	
	self fileFormatId: aClass id
]

{ #category : #accessing }
IceRepositoryProperties >> fileFormatId: aFileFormatId [
	
	self properties at: #format put: aFileFormatId
]

{ #category : #comparing }
IceRepositoryProperties >> hash [
	^ properties hash
]

{ #category : #testing }
IceRepositoryProperties >> isDirty [
	
	^ repository workingCopy referenceCommit properties ~= self
]

{ #category : #testing }
IceRepositoryProperties >> isUnborn [
	
	^ false
]

{ #category : #'private ' }
IceRepositoryProperties >> properties [
	^ properties ifNil: [ properties := Dictionary new ]
]

{ #category : #'private ' }
IceRepositoryProperties >> properties: aDictionary [
	properties := aDictionary
]

{ #category : #accessing }
IceRepositoryProperties >> propertiesFilePath [
	
	^ repository project sourceDirectoryPath / self class propertiesFileName
]

{ #category : #accessing }
IceRepositoryProperties >> readerClass [
	^ TIceRepositoryReader users detect: [ :each | each id = self fileFormat id  ]
]

{ #category : #accessing }
IceRepositoryProperties >> repository [
	^ repository
]

{ #category : #accessing }
IceRepositoryProperties >> repository: anIceLibgitRepository [ 
	repository := anIceLibgitRepository
]

{ #category : #storing }
IceRepositoryProperties >> storeOnDirectory: aFileLocator root: rootDirectory [

	| propertiesFileReference relativePath |
	propertiesFileReference := aFileLocator / self class propertiesFileName.
	relativePath := propertiesFileReference relativeTo: rootDirectory.
	
	propertiesFileReference resolve
		ensureDelete;
		writeStreamDo: [ :stream | 
			(STONWriter on: stream) 
				prettyPrint: true;
				newLine: OSPlatform current lineEnding;
				nextPut: self properties  ]
]

{ #category : #storing }
IceRepositoryProperties >> storeOnFileReference: aFileReference [

	aFileReference
		ensureDelete;
		writeStreamDo: [ :stream | 
			(STONWriter on: stream) 
				prettyPrint: true;
				newLine: OSPlatform current lineEnding;
				nextPut: self properties  ]
]

{ #category : #accessing }
IceRepositoryProperties >> writerClass [
	^ self properties
		at: 'format' 
		ifPresent: [ :v | TIceRepositoryWriter users detect: [ :each | each id = v  ] ]
		ifAbsent: [ 
			commitish project isInvalid
				ifFalse: [ IceLibgitRepository defaultFileFormat ]
				ifTrue: [ TIceRepositoryWriter users
					detect: #isLegacyDefault
					ifNone: [ Smalltalk at: #IceLibgitFiletreeWriter ] ]  ]

]
