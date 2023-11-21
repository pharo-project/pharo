"
A definition of an external projects. 
External projects are kept at `BaselineOfPharo` but instead keeping a lot of strings on it, this class is the reification of the information needed.
Is used to point to the place to download them (`PharoExternalProject>>repository`), but also to calculate the correct commit of the project (See `IcePharoPlugin class>>#commitOfExternalProject:`).
"
Class {
	#name : 'PharoExternalProject',
	#superclass : 'Object',
	#instVars : [
		'name',
		'version',
		'sourceDir',
		'project',
		'owner'
	],
	#category : 'BaselineOfPharo-Utilities',
	#package : 'BaselineOfPharo',
	#tag : 'Utilities'
}

{ #category : 'defaults' }
PharoExternalProject class >> defaultSourceDir [

	^ 'src'
]

{ #category : 'instance creation' }
PharoExternalProject class >> newName: aName owner: owner project: aProject version: aVersion [

	^ self 
		newName: aName 
		owner: owner
		project: aProject 
		version: aVersion
		sourceDir: self defaultSourceDir
]

{ #category : 'instance creation' }
PharoExternalProject class >> newName: aName owner: anOwner project: aProject version: aVersion sourceDir: aSourceDir [

	^ self basicNew
		initializeName: aName 
			owner: anOwner
			project: aProject 
			version: aVersion 
			sourceDir: aSourceDir;
		yourself
]

{ #category : 'initialization' }
PharoExternalProject >> initializeName: aName owner: anOwner project: aProject version: aVersion sourceDir: aSourceDir [

	self initialize.
	name := aName.
	owner := anOwner.
	project := aProject.
	version := aVersion.
	sourceDir := aSourceDir
]

{ #category : 'initialization' }
PharoExternalProject >> initializeName: aName project: aRepository version: aVersion sourceDir: aSourceDir [

	self initialize.
	name := aName.
	project := aRepository.
	version := aVersion.
	sourceDir := aSourceDir
]

{ #category : 'accessing' }
PharoExternalProject >> name [

	^ name
]

{ #category : 'accessing' }
PharoExternalProject >> owner [

	^ owner
]

{ #category : 'printing' }
PharoExternalProject >> printOn: aStream [

	aStream nextPutAll: self name
]

{ #category : 'accessing' }
PharoExternalProject >> project [

	^ project
]

{ #category : 'accessing' }
PharoExternalProject >> repository [

	^ 'github://{1}/{2}:{3}{4}' format: { 
		self owner.
		self project.
		self version.
		self sourceDirWithSlash }
]

{ #category : 'accessing' }
PharoExternalProject >> sourceDir [

	^ sourceDir
]

{ #category : 'private' }
PharoExternalProject >> sourceDirWithSlash [

	^ self sourceDir isEmptyOrNil
		ifTrue: [ '' ]
		ifFalse: [ '/', self sourceDir ]
]

{ #category : 'accessing' }
PharoExternalProject >> version [

	^ version
]
