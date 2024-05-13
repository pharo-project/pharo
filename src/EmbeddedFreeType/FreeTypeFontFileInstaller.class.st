"
Install Free Types Fonts from a file.
"
Class {
	#name : 'FreeTypeFontFileInstaller',
	#superclass : 'AbstractFreeTypeFontFile',
	#category : 'EmbeddedFreeType-Installation',
	#package : 'EmbeddedFreeType',
	#tag : 'Installation'
}

{ #category : 'instance creation' }
FreeTypeFontFileInstaller class >> withFileInfoProvider: anEmbeddedFreeTypeFontInstaller [

	^ self new fileInfoProvider: anEmbeddedFreeTypeFontInstaller 
]

{ #category : 'adding' }
FreeTypeFontFileInstaller >> addAndCacheFileInfoFromBytes: bytes filename: originalFileBaseName [

	| info |
	info := self fileInfoFromBytes: bytes filename: originalFileBaseName.
	self fileInfoProvider addFirstFileInfo: info index: index.
	self fileInfoProvider cacheEmbeddedFileInfo: info index: index
]

{ #category : 'accessing' }
FreeTypeFontFileInstaller >> addFromFileContents: bytes baseName: originalFileBaseName [

	| externalMem cachedNumFaces |
	index := 0.
	cachedNumFaces := self cachedNumFacesFromData: bytes.
	(self isAlreadyCached: cachedNumFaces) ifTrue: [ ^ self ].
	
	[
	externalMem := FreeTypeExternalMemory bytes: bytes.
	externalMem validate.
	face := self newFaceFromData: externalMem.
	self isValidFace
		ifFalse: [
			self failedToOpen: face index: index.
			^ self ]
		ifTrue: [
			numFaces ifNil: [ numFaces := face numFaces ].
			self addAndCacheFileInfoFromBytes: bytes filename: originalFileBaseName ].
	index := index + 1.
	index < numFaces ] whileTrue
]

{ #category : 'accessing' }
FreeTypeFontFileInstaller >> cachedNumFacesFromData: bytes [

	| cachedInfo cachedNumFaces |
	[
	(cachedInfo := self fileInfoProvider
		               validEmbeddedCachedInfoFor: bytes
		               index: index) isNotNil ] whileTrue: [
		index = 0 ifTrue: [ cachedNumFaces := cachedInfo numFaces ].
		self fileInfoProvider addFirstFileInfo: cachedInfo index: index.
		index := index + 1 ].
	^ cachedNumFaces
]

{ #category : 'error handling' }
FreeTypeFontFileInstaller >> failedToOpen: anFace index: anIndex [
	"Transcript cr; show: 'Failed : ', path asString, '[', i asString,']'."
	"remove all cache entries for path with index >= i"
]

{ #category : 'accessing' }
FreeTypeFontFileInstaller >> fileInfoFromBytes: bytes filename: originalFileBaseName [

	^ FreeTypeEmbeddedFileInfo new
		  baseName: originalFileBaseName;
		  fileContents: bytes;
		  index: index;
		  familyName: face familyName;
		  styleName: face styleName;
		  postscriptName: face postscriptName;
		  bold: face isBold;
		  italic: face isItalic;
		  fixedWidth: face isFixedWidth;
		  numFaces: numFaces;
		  extractAttributesFromNames;
		  yourself
]

{ #category : 'accessing' }
FreeTypeFontFileInstaller >> newFaceFromData: externalMem [

	| newFace |
	newFace := FreeTypeFace basicNew fileContentsExternalMemory:
		           externalMem.
	^ [
	  newFace
		  newFaceFromExternalMemory: externalMem index: index;
		  loadFields;
		  yourself ]
		  on: FT2Error , PrimitiveFailed
		  do: [ :e | nil ]
]
