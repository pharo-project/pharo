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
	#name : #EmbeddedFreeTypeFontInstaller,
	#superclass : #Object,
	#instVars : [
		'embeddedFileInfoCache',
		'provider'
	],
	#classInstVars : [
		'current'
	],
	#category : #EmbeddedFreeType
}

{ #category : #accessing }
EmbeddedFreeTypeFontInstaller class >> current [
	current ifNil: [ 
		current := self new 
			provider: FreeTypeFontProvider current; 
			yourself.
		FreeTypeFontProvider current addFontInstaller: self current ].
	^ current.
]

{ #category : #'class initialization' }
EmbeddedFreeTypeFontInstaller class >> initialize [
	super initialize.
	self resetCurrent.
	self current.
]

{ #category : #'extra fonts registration' }
EmbeddedFreeTypeFontInstaller class >> registerFont: aClassWithFont [
	"Do nothing if freetype is disabled currently"
	FreeTypeSystemSettings loadFt2Library 
		ifTrue: [  aClassWithFont installFontsIn: self current ]
]

{ #category : #accessing }
EmbeddedFreeTypeFontInstaller class >> resetCurrent [
	| tmpCurrent |
	current ifNil: [ ^ self ].
	tmpCurrent := current.
	current := nil.
	FreeTypeFontProvider current removeFontInstaller: tmpCurrent.
]

{ #category : #adding }
EmbeddedFreeTypeFontInstaller >> addFirstFileInfo: aFreeTypeEmbeddedFileInfo index: anInteger [ 
	provider addFirstFileInfo: aFreeTypeEmbeddedFileInfo index: anInteger 
]

{ #category : #accessing }
EmbeddedFreeTypeFontInstaller >> addFromFileContents: bytes baseName:  originalFileBaseName [
	| i face numFaces info externalMem cachedInfo cachedNumFaces |

	i:= 0.
	[(cachedInfo := self validEmbeddedCachedInfoFor: bytes index: i) notNil]
		whileTrue:[
			i = 0 ifTrue:[cachedNumFaces := cachedInfo numFaces].
			self addFirstFileInfo: cachedInfo index: i.
			i := i + 1.].
	(cachedNumFaces notNil and:[i >= cachedNumFaces]) ifTrue:[^self].
	[externalMem := FreeTypeExternalMemory bytes: bytes.
	externalMem validate.
	face := FreeTypeFace basicNew fileContentsExternalMemory: externalMem .
	[ "we use the primNewFaceFromFile:index: method because we want to do this as fast as possible and we don't need the face registered because it will be explicitly destroyed later"
	face newFaceFromExternalMemory: externalMem index: i.
	face loadFields] 
		on: FT2Error 
		do:[:e | 
			self failedToOpen:face index: i.
			^ self].
	(face height notNil  and:[face hasFamilyName and:[face hasStyleName and:[face isValid]]])
		ifFalse:[
			self failedToOpen:face index: i.
			^ self]
		ifTrue:[ 
			numFaces ifNil: [numFaces := face numFaces].
			info := FreeTypeEmbeddedFileInfo new
				baseName: originalFileBaseName;
				fileContents: bytes;
				index: i;
				familyName: face familyName;
				styleName: face styleName;
				postscriptName: face postscriptName;
				bold: face isBold;
				italic: face isItalic;
				fixedWidth: face isFixedWidth;
				numFaces: numFaces;
				extractAttributesFromNames;
				yourself.
			self addFirstFileInfo: info index: i.
			self cacheEmbeddedFileInfo: info index: i.
			"Transcript show: 'from file : ', info asString."].
	i := i + 1.
	i < numFaces "note, we use < rather than <= , because i is zero based"] whileTrue:[].
]

{ #category : #accessing }
EmbeddedFreeTypeFontInstaller >> cacheEmbeddedFileInfo: aFreeTypeEmbeddedFileInfo index: i [

	(embeddedFileInfoCache at:{aFreeTypeEmbeddedFileInfo fileSize. i} ifAbsentPut:[ Set new ])
		add: aFreeTypeEmbeddedFileInfo
]

{ #category : #accessing }
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

{ #category : #'error handling' }
EmbeddedFreeTypeFontInstaller >> failedToOpen: face index: index [
	"Transcript cr; show: 'Failed : ', path asString, '[', i asString,']'."
	"remove all cache entries for path with index >= i"
]

{ #category : #initialization }
EmbeddedFreeTypeFontInstaller >> initialize [
	super initialize.
	embeddedFileInfoCache := Dictionary new: 10. "keyed by file size" 	
]

{ #category : #accessing }
EmbeddedFreeTypeFontInstaller >> installAllFontsIn: aProvider [
	self provider: aProvider.
	EmbeddedFreeTypeFontFontDescription installAllFontsIn: self.	
	embeddedFileInfoCache valuesDo: [ :eachSet | eachSet do: [ :each | provider addFileInfo: each ] ]
]

{ #category : #accessing }
EmbeddedFreeTypeFontInstaller >> provider: aProvider [
	provider := aProvider
]

{ #category : #accessing }
EmbeddedFreeTypeFontInstaller >> validEmbeddedCachedInfoFor: bytes index: i [
	"answer info from cache if the bytes are the same as the cached info, otherwise answer nil"
	| cacheEntry fileSize |

	fileSize := bytes size.
	cacheEntry := (embeddedFileInfoCache at: {fileSize. i} ifAbsentPut:[Set new])
		detect:[:each | each fileContents = bytes]
		ifNone:[].
	^cacheEntry
]
