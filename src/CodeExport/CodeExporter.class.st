"
An utility class for exporting code to changesets (*.cs) or Smalltalk source (*.st) files
"
Class {
	#name : #CodeExporter,
	#superclass : #Object,
	#category : #'CodeExport-Base'
}

{ #category : #'file-out' }
CodeExporter class >> writeSourceCodeFrom: aStream baseName: baseName isSt: stOrCsFlag [

	| extension targetFile |
	extension := stOrCsFlag ifTrue: ['st']  ifFalse: ['cs'].
	targetFile := FileLocator imageDirectory / baseName asFileName, extension.

	self writeSourceCodeFrom: aStream toFileReference: targetFile
]

{ #category : #'file-out' }
CodeExporter class >> writeSourceCodeFrom: aStream toFileReference: aFileReference [
	"Write the supplied changes to aFileReference"

	aFileReference binaryWriteStreamDo: [ :outputStream |
		(ZnCharacterWriteStream on: outputStream encoding: 'utf8')
			nextPutAll: aStream contents.
	].

	self inform: 'Filed out to: ', String cr, aFileReference pathString
]
