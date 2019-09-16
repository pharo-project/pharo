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

	aFileReference
		ensureDelete;
		binaryWriteStreamDo: [ :outputStream | (ZnCharacterWriteStream on: outputStream encoding: 'utf8') nextPutAll: aStream contents ].

	UIManager default
		inform:
			(String
				streamContents: [ :s | 
					s
						nextPutAll: 'Filed out to: ';
						cr;
						nextPutAll: aFileReference pathString;
						cr;
						cr;
						nextPutAll: 'Click to open location' ])
		actionOnClick: [ aFileReference openInOSFileBrowser ]
]
