"
Implements chunk export for Epicea entries to a file.
"
Class {
	#name : 'EpChunkExporter',
	#superclass : 'Object',
	#instVars : [
		'outputFilename',
		'application'
	],
	#category : 'EpiceaBrowsers-Exporter',
	#package : 'EpiceaBrowsers',
	#tag : 'Exporter'
}

{ #category : 'instance creation' }
EpChunkExporter class >> fileOut: entries [
	"Write Epicea entries in the current working directory"

	self new fileOut: entries
]

{ #category : 'instance creation' }
EpChunkExporter class >> fileOut: entries in: aFileReference [
	"Write the Epicea entries in aFileReference"

	self new
		fileOut: entries
		in: aFileReference
]

{ #category : 'accessing' }
EpChunkExporter >> application: anApp [

	application := anApp
]

{ #category : 'defaults' }
EpChunkExporter >> defaultOutputFilename [
	"Answer a default <String> which will be the file name when a file out is requested"
	
	^ String streamContents: [ :stream |
		| dateAndTime |
		dateAndTime := DateAndTime now.
		stream 
			nextPutAll: 'EpLog-'.
		dateAndTime printYMDOn: stream.
		stream print: dateAndTime hours.
		stream nextPut: $-.
		stream print: dateAndTime minutes.
		stream nextPutAll: '.st' ].
]

{ #category : 'accessing' }
EpChunkExporter >> fileOut: epiceaEntries [
	"Write the receiver's entries in the current working directory"

	self fileOut: epiceaEntries in: FileLocator workingDirectory
]

{ #category : 'accessing' }
EpChunkExporter >> fileOut: epiceaEntries in: aDirectory [
	
	"(application newPresenter: StSaveFilePresenter) is probably the iconic way to create a presenter
	but it did not work and I have no idea why."
	
	| fileRef dialog ok |
	self flag: #todo. 
	StSaveFilePresenter new
		defaultFolder: aDirectory;
		nameText:  self outputFilename; 
		okAction: [ :fileReference |
				fileRef := fileReference.
				fileRef writeStreamDo: [ :s | 
					self internalFileOutOf: epiceaEntries in: s ].
				dialog := SpConfirmDialog new.
	ok := dialog
		      title: 'Do you want to browse natively the fileout?';
		      label: 'Are you sure?';
		      acceptLabel: 'Sure!';
		      cancelLabel: 'No, forget it';
		      openModal.
	ok ifTrue: [ fileRef openInOSFileBrowser ]
				];
		openModal.
	
]

{ #category : 'private' }
EpChunkExporter >> internalFileOutOf: epiceaEntries in: aStream [

	epiceaEntries do: [ :epEntryItem | epEntryItem fileOutEntryOn: aStream ] 
]

{ #category : 'accessing' }
EpChunkExporter >> outputFileReference [
	"Answer the receiver's output file as a <FileReference>"
	
	^ self outputFilename asFileReference
]

{ #category : 'accessing' }
EpChunkExporter >> outputFilename [
	"Answer a <String> which will be the file name when a file out is requested"
	
	^ outputFilename 
		ifNil: [ outputFilename := self defaultOutputFilename ]
]

{ #category : 'accessing' }
EpChunkExporter >> outputFilename: anObject [

	outputFilename := anObject
]
