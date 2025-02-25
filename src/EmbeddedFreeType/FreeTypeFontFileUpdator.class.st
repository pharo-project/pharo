"
update Free Types Fonts from a file.
"
Class {
	#name : 'FreeTypeFontFileUpdator',
	#superclass : 'AbstractFreeTypeFontFile',
	#category : 'EmbeddedFreeType-Installation',
	#package : 'EmbeddedFreeType',
	#tag : 'Installation'
}

{ #category : 'instance creation' }
FreeTypeFontFileUpdator class >> withFileInfoProvider: anFreeTypeFontProvider [

	^ self new fileInfoProvider: anFreeTypeFontProvider 
]

{ #category : 'loading and updating' }
FreeTypeFontFileUpdator >> addAndCacheFileInfoFromPath: path file: aFile [

	| info |
	info := self fileInfoFromPath: path file: aFile.
	self fileInfoProvider addFileInfo: info index: index.
	self fileInfoProvider cacheFileInfo: info index: index
]

{ #category : 'accessing' }
FreeTypeFontFileUpdator >> cachedNumFacesFromData: bytes [

	| cachedInfo cachedNumFaces |
	[
	(cachedInfo := self fileInfoProvider
		               validCachedInfoFor: bytes
		               index: index) isNotNil ] whileTrue: [
		index = 0 ifTrue: [ cachedNumFaces := cachedInfo numFaces ].
		self fileInfoProvider addFirstFileInfo: cachedInfo index: index.
		index := index + 1 ].
	^ cachedNumFaces
]

{ #category : 'error handling' }
FreeTypeFontFileUpdator >> failedToOpen: aFace from: aPath index: anIndex [
	"Transcript cr; show: 'Failed : ', path asString, '[', i asString,']'."
	"remove all cache entries for path with index >= i"
]

{ #category : 'loading and updating' }
FreeTypeFontFileUpdator >> fileInfoFromPath: path file: aFile [

	^ FreeTypeFileInfo new
		  absoluteOrRelativePath: aFile path isAbsolute;
		  absolutePath: path;
		  "used for quick lookup on same platform"locationType: #default;
		  index: index;
		  fileSize: aFile size;
		  modificationTime: aFile modificationTime;
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

{ #category : 'instance creation' }
FreeTypeFontFileUpdator >> newFaceFromPath: path [

	| newFace |
	newFace := FreeTypeFace basicNew
		           filename: path;
		           index: index.
	^ [
	  newFace newFaceFromFile: path index: index.
	  newFace loadFields.
	  newFace ]
		  on: FT2Error , PrimitiveFailed
		  do: [ :e | nil ]
]

{ #category : 'loading and updating' }
FreeTypeFontFileUpdator >> updateFromFile: aFile [

	| cachedNumFaces path |
	index := 0.
	cachedNumFaces := self cachedNumFacesFromData: aFile.

	(self isAlreadyCached: cachedNumFaces) ifTrue: [ ^ self ].

	path := aFile fullName utf8Encoded asString.
	[
	face := self newFaceFromPath: path.
	self isValidFace
		ifFalse: [ ^ self failedToOpen: face from: path index: index ]
		ifTrue: [
			numFaces ifNil: [ numFaces := face numFaces ].
			self addAndCacheFileInfoFromPath: path file: aFile ].
	index := index + 1.
	index < numFaces ] whileTrue
]
