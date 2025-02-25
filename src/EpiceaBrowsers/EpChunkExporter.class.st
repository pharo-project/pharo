"
Implements chunk export for Epicea entries to a file.
"
Class {
	#name : 'EpChunkExporter',
	#superclass : 'Object',
	#instVars : [
		'outputFilename'
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

{ #category : 'defaults' }
EpChunkExporter >> defaultOutputFilename [
	"Answer a default <String> which will be the file name when a file out is requested"
	
	^ String streamContents: [ : stream |
		stream 
			nextPutAll: 'EpLog-';
			nextPutAll: DateAndTime now asFileNameCompatibleString;
			nextPutAll: '.st' ].
]

{ #category : 'accessing' }
EpChunkExporter >> fileOut: epiceaEntries [
	"Write the receiver's entries in the current working directory"

	self fileOut: epiceaEntries in: FileLocator workingDirectory
]

{ #category : 'accessing' }
EpChunkExporter >> fileOut: epiceaEntries in: aDirectory [

	| internalStream |

	internalStream := (String new: 1000) writeStream.
	internalStream timeStamp; cr.
	epiceaEntries do: [ : epEntryItem |
		self 
			fileOutSelectedEntry: epEntryItem 
			on: internalStream ].

	CodeExporter 
		writeSourceCodeFrom: internalStream 
		toFileReference: aDirectory / self outputFilename.
]

{ #category : 'private' }
EpChunkExporter >> fileOutSelectedEntry: anOmEntry on: aWriteStream [ 
	"Write the contents of the method modification to aWriteStream in chunk format"

	anOmEntry fileOutEntryOn: aWriteStream
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
