"
An utility class for exporting code to changesets (*.cs) or Smalltalk source (*.st) files
"
Class {
	#name : 'CodeExporter',
	#superclass : 'Object',
	#category : 'CodeExport-Base',
	#package : 'CodeExport',
	#tag : 'Base'
}

{ #category : 'file-out' }
CodeExporter class >> writeSourceCodeFrom: aStream baseName: baseName isSt: stOrCsFlag [
	"Write aStream as a file in the image directory. baseName should be a valid file name <String>.
	If stOrCsFlag is <true>, then append '.st' extension, otherwise use '.cs' extension"

	| extension targetFileReference |
	
	extension := stOrCsFlag ifTrue: ['.st'] ifFalse: ['.cs'].
	targetFileReference := (baseName , extension) asFileReference.

	self 
		writeSourceCodeFrom: aStream 
		toFileReference: FileLocator imageDirectory / targetFileReference
]

{ #category : 'file-out' }
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
