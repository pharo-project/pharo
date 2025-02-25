"
An EmbeddedFreeTypeFontInstaller installs embedded free type fonts in a provider.
It registers itself as a font installer to FreeTypeFontProvider. And when someone asks to update fonts from system, EmbeddedFreeTypeFontInstaller installs corresponding fonts.

Instance Variables
	embeddedFileInfoCache:		<Dictionary>
	provider:		<FreeTypeFontProvider>

embeddedFileInfoCache
	- internal cache

provider
	- a provider which asks for installing the embedded fonts.
"
Class {
	#name : 'EmbeddedFreeTypeFontInstaller',
	#superclass : 'Object',
	#instVars : [
		'embeddedFileInfoCache',
		'provider',
		'numFaces',
		'face'
	],
	#classInstVars : [
		'current'
	],
	#category : 'EmbeddedFreeType-Installation',
	#package : 'EmbeddedFreeType',
	#tag : 'Installation'
}

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller class >> current [
	current ifNil: [
		current := self new
			provider: FreeTypeFontProvider current;
			yourself.
		FreeTypeFontProvider current addFontInstaller: self current ].
	^ current
]

{ #category : 'class initialization' }
EmbeddedFreeTypeFontInstaller class >> initialize [

	self
		resetCurrent;
		current
]

{ #category : 'extra fonts registration' }
EmbeddedFreeTypeFontInstaller class >> registerFont: aClassWithFont [
	"Do nothing if freetype is disabled currently"
	FreeTypeSystemSettings loadFt2Library
		ifTrue: [  aClassWithFont installFontsIn: self current ]
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller class >> resetCurrent [
	| tmpCurrent |
	current ifNil: [ ^ self ].
	tmpCurrent := current.
	current := nil.
	FreeTypeFontProvider current removeFontInstaller: tmpCurrent
]

{ #category : 'adding' }
EmbeddedFreeTypeFontInstaller >> addFirstFileInfo: aFreeTypeEmbeddedFileInfo index: anInteger [
	provider addFirstFileInfo: aFreeTypeEmbeddedFileInfo index: anInteger
]

{ #category : 'adding' }
EmbeddedFreeTypeFontInstaller >> addFromFileContents: bytes baseName: originalFileBaseName [

	(FreeTypeFontFileInstaller withFileInfoProvider: self)
		addFromFileContents: bytes
		baseName: originalFileBaseName
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> cacheEmbeddedFileInfo: aFreeTypeEmbeddedFileInfo index: i [

	(embeddedFileInfoCache at:{aFreeTypeEmbeddedFileInfo fileSize. i} ifAbsentPut:[ Set new ])
		add: aFreeTypeEmbeddedFileInfo
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> embedFilesInDirectory: aDirectory [
	"embed all the files in aDirectory
	EmbeddedFreeTypeFontInstaller new
		provider: FreeTypeFontProvider current;
		embedFilesInDirectory: (FileSystem disk workingDirectory / 'Fonts')
	"

	aDirectory files do:[:file |
		file readStreamDo: [ :fileStream|.
			fileStream binary.
			self
				addFromFileContents: fileStream contents
				baseName: file basename]].

	"update so that missing text styles are created."
	self updateFromSystem.

	"clear all the logicalFonts realFonts so that embedded fonts take precedence over external ones"
	LogicalFont allInstances do:[:logFont | logFont clearRealFont]
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> face [

	^ face
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> face: anObject [

	face := anObject
]

{ #category : 'initialization' }
EmbeddedFreeTypeFontInstaller >> initialize [
	super initialize.
	embeddedFileInfoCache := Dictionary new: 10. "keyed by file size"
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> installAllFontsIn: aProvider [
	self provider: aProvider.
	EmbeddedFreeTypeFontDescription installAllFontsIn: self.
	embeddedFileInfoCache valuesDo: [ :eachSet | eachSet do: [ :each | provider addFileInfo: each ] ]
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> numFaces [

	^ numFaces
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> numFaces: anObject [

	numFaces := anObject
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> provider: aProvider [
	provider := aProvider
]

{ #category : 'accessing' }
EmbeddedFreeTypeFontInstaller >> validEmbeddedCachedInfoFor: bytes index: i [
	"answer info from cache if the bytes are the same as the cached info, otherwise answer nil"
	| cacheEntry fileSize |

	fileSize := bytes size.
	cacheEntry := (embeddedFileInfoCache at: {fileSize. i} ifAbsentPut:[Set new])
		detect:[:each | each fileContents = bytes]
		ifNone:[].
	^cacheEntry
]
