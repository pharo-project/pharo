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
		description: 'The path of a directory where Pharo will store local files' ;
		label: 'Local directory'.
]

{ #category : #accessing }
SystemResolver class >> userLocalDirectory [
	^ UserLocalDirectory ifNil: [ 
		(FileLocator imageDirectory / self defaultLocalDirectoryName) resolve  ]

]

{ #category : #accessing }
SystemResolver class >> userLocalDirectory: aFileReference [
	UserLocalDirectory := aFileReference
]

{ #category : #origins }
SystemResolver >> changes [
	^ self image withExtension: Smalltalk changesSuffix
]

{ #category : #origins }
SystemResolver >> image [
	^ self resolveString: Smalltalk primImagePath
]

{ #category : #origins }
SystemResolver >> imageDirectory [
	^ self image parent
]

{ #category : #origins }
SystemResolver >> localDirectory [ 
	^ self class userLocalDirectory 
		ifNil: [ (self imageDirectory / self class defaultLocalDirectoryName) ensureCreateDirectory ]
]

{ #category : #resolving }
SystemResolver >> supportedOrigins [
	^ #(image imageDirectory changes vmBinary vmDirectory workingDirectory localDirectory)
]

{ #category : #origins }
SystemResolver >> vmBinary [
	^ self resolveString: Smalltalk vm fullPath
]

{ #category : #origins }
SystemResolver >> vmDirectory [
	^ self resolveString: Smalltalk vm primVmDirectoryPath
]
