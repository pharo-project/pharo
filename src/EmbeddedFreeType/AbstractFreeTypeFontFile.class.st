"
Abstract superclass for a FreeType font installer/updator
"
Class {
	#name : 'AbstractFreeTypeFontFile',
	#superclass : 'Object',
	#instVars : [
		'fileInfoProvider',
		'face',
		'numFaces',
		'index'
	],
	#category : 'EmbeddedFreeType-Installation',
	#package : 'EmbeddedFreeType',
	#tag : 'Installation'
}

{ #category : 'testing' }
AbstractFreeTypeFontFile class >> isAbstract [

	^ self == AbstractFreeTypeFontFile
]

{ #category : 'accessing' }
AbstractFreeTypeFontFile >> cachedNumFacesFromData: bytes [

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

{ #category : 'accessing' }
AbstractFreeTypeFontFile >> fileInfoProvider [

	^ fileInfoProvider
]

{ #category : 'accessing' }
AbstractFreeTypeFontFile >> fileInfoProvider: anObject [

	fileInfoProvider := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFontFile >> isAlreadyCached: cachedNumFaces [

	^ cachedNumFaces isNotNil and: [ index >= cachedNumFaces ]
]

{ #category : 'accessing' }
AbstractFreeTypeFontFile >> isValidFace [

	^ face isNotNil and: [
		  face height isNotNil and: [
			  face hasFamilyName and: [
				  face hasStyleName and: [ face isValid ] ] ] ]
]
