"
I resolve origins that are related to the currently running Smalltalk system, using primitives provided by the VM. 
"
Class {
	#name : #SystemResolver,
	#superclass : #FileSystemResolver,
	#classVars : [
		'UserLocalDirectory'
	],
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #accessing }
SystemResolver class >> defaultLocalDirectory [
	^ (FileLocator imageDirectory / self defaultLocalDirectoryName)
		resolve
]

{ #category : #accessing }
SystemResolver class >> defaultLocalDirectoryName [
	^ 'pharo-local'
]

{ #category : #settings }
SystemResolver class >> settingsOn: aBuilder [
	<systemsettings>  

	(aBuilder setting: #userLocalDirectory)
		parent: #pharoSystem;
		type: #Directory;
		target: self;
		default: self defaultLocalDirectory;
		description: 'The path of a directory where Pharo will store local files' ;
		label: 'Local directory'.
]

{ #category : #accessing }
SystemResolver class >> userLocalDirectory [

	^ UserLocalDirectory ifNil: [ self defaultLocalDirectory ]
]

{ #category : #accessing }
SystemResolver class >> userLocalDirectory: aFileReferenceOrPathName [
	|aDirReference| 
	aDirReference := aFileReferenceOrPathName asFileReference.
	aDirReference isDirectory ifTrue: [UserLocalDirectory := aDirReference ]
]

{ #category : #origins }
SystemResolver >> changes [
	<origin>
	^ self image withExtension: Smalltalk changesSuffix
]

{ #category : #origins }
SystemResolver >> image [
	<origin>
	^ self resolveString: Smalltalk imagePath
]

{ #category : #origins }
SystemResolver >> imageDirectory [
	<origin>
	^ self image parent
]

{ #category : #origins }
SystemResolver >> localDirectory [ 
	<origin>
	^ self class userLocalDirectory 
		ifNil: [ (self imageDirectory / self class defaultLocalDirectoryName) ensureCreateDirectory ]
]

{ #category : #origins }
SystemResolver >> vmBinary [
	<origin>
	^ self resolveString: Smalltalk vm fullPath
]

{ #category : #origins }
SystemResolver >> vmDirectory [
	<origin>
	^ self resolveString: Smalltalk vm directory
]
