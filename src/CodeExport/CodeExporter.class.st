Class {
	#name : #CodeExporter,
	#superclass : #Object,
	#category : #CodeExport
}

{ #category : #'file-out' }
CodeExporter class >> writeSourceCodeFrom: aStream baseName: baseName isSt: stOrCsFlag [

	| extension fileName  outputStream |
	extension := stOrCsFlag ifTrue: ['.st']  ifFalse: ['.cs'].
	fileName := baseName, extension.
	fileName := FileSystem disk checkName: fileName fixErrors: true.
	outputStream := (File named: fileName) writeStream.
	
	(ZnCrPortableWriteStream on: (ZnCharacterWriteStream
		on: outputStream
		encoding: 'utf8')) nextPutAll: aStream contents.

	outputStream close.

	self inform: 'Filed out to: ', String cr, fileName.
]

{ #category : #'file-out' }
CodeExporter class >> writeSourceCodeFrom: aStream toFileReference: aFileReference [

	aFileReference writeStreamDo: [ :outputStream |
		(ZnCrPortableWriteStream on: (ZnCharacterWriteStream
			on: outputStream
			encoding: 'utf8')) nextPutAll: aStream contents.
	].

	self inform: 'Filed out to: ', String cr, aFileReference basename
]
