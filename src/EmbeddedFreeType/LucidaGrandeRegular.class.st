"
I'm a stub to load Lucida Grande font in system. 
As is, it works just for macs, but the idea is to use it for showing shortcut symbols... which has sense just un mac :)
"
Class {
	#name : #LucidaGrandeRegular,
	#superclass : #EmbeddedFreeTypeFontFontDescription,
	#category : #EmbeddedFreeType
}

{ #category : #testing }
LucidaGrandeRegular class >> canBeInstalled [ 
	"This font can be installed just in Macs *and* if font exists"
	^ OSPlatform current isMacOS and: [ self fontReference exists ]
]

{ #category : #accessing }
LucidaGrandeRegular class >> fontContents [
	^ self fontReference 
		binaryReadStreamDo: [ :stream | stream contents ]
	
	
]

{ #category : #accessing }
LucidaGrandeRegular class >> fontDirectory [
	"Lucida Grande comes in mac system, at that directory. 
	 Not happy with the hardcoding, but it should not change"
	^ '/System/Library/Fonts' asFileReference
]

{ #category : #accessing }
LucidaGrandeRegular class >> fontReference [ 
	^ self fontDirectory / self originalFileName
	
]

{ #category : #'class initialization' }
LucidaGrandeRegular class >> initialize [ 
	self canBeInstalled 
		ifTrue: [ EmbeddedFreeTypeFontInstaller registerFont: self ]
]

{ #category : #accessing }
LucidaGrandeRegular class >> originalFileName [
	^ 'LucidaGrande.ttc'
]
