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

{ #category : 'instance creation' }
AbstractFreeTypeFontFile class >> withFileInfoProvider: anFreeTypeFontProvider [ 
	^ self subclassResponsibility
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
