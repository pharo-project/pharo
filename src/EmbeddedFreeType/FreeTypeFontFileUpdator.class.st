Class {
	#name : 'FreeTypeFontFileUpdator',
	#superclass : 'FreeTypeFontFileInstaller',
	#category : 'EmbeddedFreeType-Installation',
	#package : 'EmbeddedFreeType',
	#tag : 'Installation'
}

{ #category : 'error handling' }
FreeTypeFontFileUpdator >> failedToOpen: face from: path index: i [
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

{ #category : 'loading and updating' }
FreeTypeFontFileUpdator >> updateFromFile: aFile [

	| info cachedNumFaces path |
	index := 0.
	cachedNumFaces := self cachedNumFacesFromData: aFile.

	(self isAlreadyCached: cachedNumFaces) ifTrue: [ ^ self ].

	path := aFile fullName utf8Encoded asString.
	[
	[face := FreeTypeFace basicNew filename: path; index: index.
	face newFaceFromFile: path index: index.
	face loadFields]
		on: FT2Error, PrimitiveFailed
		do: [:e | ^self failedToOpen: face from: path index: index ].
	(self isValidFace )
		ifFalse: [ ^ self failedToOpen: face from: path index: index ]
		ifTrue: [
			numFaces ifNil: [ numFaces := face numFaces ].
			info := self fileInfoFromPath: path file: aFile. 
			self fileInfoProvider addFileInfo: info index: index.
			self fileInfoProvider cacheFileInfo: info index: index
			"Transcript show: 'from file : ', info asString." ].
	index := index + 1.
	index < numFaces "note, we use < rather than <= , because i is zero based" ]
		whileTrue: [  ]
]
