Class {
	#name : #IceBasicProject,
	#superclass : #IceAbstractProject,
	#instVars : [
		'properties',
		'repository',
		'repositoryProperties'
	],
	#category : #'Iceberg-Project'
}

{ #category : #'instance creation' }
IceBasicProject class >> onProperties: properties commitish: aCommitish [
	
	^ self basicNew
		properties: properties;
		commitish: aCommitish;
		initialize;
		yourself
]

{ #category : #'instance creation' }
IceBasicProject class >> onRepository: aRepository [
	
	^ self basicNew
		properties: Dictionary new;
		commitish: aRepository workingCopy;
		initialize;
		yourself
]

{ #category : #'instance creation' }
IceBasicProject class >> onRepository: aRepository withSourceDirectory: aDirectory [
	
	^ self basicNew
		properties: Dictionary new;
		commitish: aRepository workingCopy;
		sourceDirectory: aDirectory;
		initialize;
		yourself
]

{ #category : #comparing }
IceBasicProject >> = anotherProject [

	self species = anotherProject species ifFalse: [ ^ false ].
	properties = anotherProject properties ifFalse: [ ^ false ].
	self repository = anotherProject repository ifFalse: [ ^ false ]. 

	^ true.
]

{ #category : #writing }
IceBasicProject >> addProjectFileToChangeBuilder: anIceChangeImporter [ 
	
	| projectFileName |
	self isUnborn ifTrue: [ ^ self ].
	
	self assert: self projectFilePath size = 1.
	projectFileName := self projectFilePath segments first.
	
	anIceChangeImporter
		addFileNamed: projectFileName
		path: self projectFilePath
		contents: self contentsString.
]

{ #category : #writing }
IceBasicProject >> addToChangeBuilder: anIceChangeImporter [

	self addProjectFileToChangeBuilder: anIceChangeImporter.
	"self addPropertiesFileToChangeBuilder: anIceChangeImporter."
]

{ #category : #initialize }
IceBasicProject >> backwardsCompatibleFormat [

	^ TIceRepositoryWriter users
		detect: #isLegacyDefault
		ifNone: [ Smalltalk at: #IceLibgitFiletreeWriter ]
]

{ #category : #accessing }
IceBasicProject >> commitish [
	^ commitish
]

{ #category : #storing }
IceBasicProject >> contentsString [

	^ ByteArray streamContents: [ :stream |
		(STONWriter on: (ZnCharacterWriteStream on: stream)) 
			prettyPrint: true;
			newLine: OSPlatform current lineEnding;
			nextPut: self properties ]
]

{ #category : #accessing }
IceBasicProject >> fileFormat: aFileFormat [

	self fileFormatId: aFileFormat id
]

{ #category : #accessing }
IceBasicProject >> fileFormatId: aFileFormat [

	self repositoryProperties fileFormatId: aFileFormat
]

{ #category : #accessing }
IceBasicProject >> fileSystem [
	
	^ commitish fileSystem
]

{ #category : #comparing }
IceBasicProject >> hash [

	^ commitish hash
]

{ #category : #initialize }
IceBasicProject >> initialize [
	super initialize.
"	repositoryProperties := self readProperties.
	repositoryProperties properties
		at: #format
		ifAbsent: [ 	repositoryProperties fileFormat: self backwardsCompatibleFormat ]"
]

{ #category : #accessing }
IceBasicProject >> name [
	
	^ self repository name
]

{ #category : #accessing }
IceBasicProject >> projectFileIn: aCommitish [ 
	
	^ aCommitish fileSystem resolve: self projectFilePath
]

{ #category : #accessing }
IceBasicProject >> projectFilePath [

	^ RelativePath with: '.project'.
]

{ #category : #accessing }
IceBasicProject >> projectFileReference [

	^ self projectFileIn: commitish
]

{ #category : #accessing }
IceBasicProject >> projectName [
	
	^ properties at: #projectName ifAbsent: [ self repository name ]
]

{ #category : #accessing }
IceBasicProject >> properties [
	^ properties
]

{ #category : #accessing }
IceBasicProject >> properties: aCollection [ 
	properties := aCollection
]

{ #category : #accessing }
IceBasicProject >> repository [
	^ commitish repository
]

{ #category : #accessing }
IceBasicProject >> repositoryProperties [
	^ self repository properties
]

{ #category : #accessing }
IceBasicProject >> sourceDirectory [
	
	^ properties at: 'srcDirectory' ifAbsent: [ 'src' ]
]

{ #category : #accessing }
IceBasicProject >> sourceDirectory: aString [ 
	
	properties at: 'srcDirectory' put: aString
]

{ #category : #accessing }
IceBasicProject >> sourceDirectoryPath [

	"API compatible both with Pharo6 and Pharo7.
	This could be replaced by the Pharo7 API
	
	RelativePath with: self subdirectory
	"
	^ (RelativePath withAll: (RelativePath canonicalizeElements: ($/ split: self sourceDirectory)))
]
