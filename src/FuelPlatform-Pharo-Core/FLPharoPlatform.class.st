Class {
	#name : #FLPharoPlatform,
	#superclass : #FLPlatform,
	#category : #'FuelPlatform-Pharo-Core-FuelPlatform-Pharo-Core'
}

{ #category : #testing }
FLPharoPlatform class >> addNativeBoostComponents [
	Smalltalk
		at: #NBExternalHandle
		ifPresent: [ :handle | ^ self ].
		
	Object variableByteSubclass: #NBExternalHandle
		instanceVariableNames: ''
		classVariableNames: ''
		category: self hacksCategoryName
]

{ #category : #testing }
FLPharoPlatform class >> addPackageManifest [
	Smalltalk
		at: #PackageManifest
		ifPresent: [ ^ self ].
	
	Object subclass: #PackageManifest
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self hacksCategoryName
]

{ #category : #'private-hacks' }
FLPharoPlatform class >> basicAddHacks [
	super basicAddHacks.
	
	self
		addNativeBoostComponents;
		addPackageManifest
]

{ #category : #testing }
FLPharoPlatform class >> isResponsibleForCurrentPlatform [
	^ SystemVersion current version beginsWith: 'Pharo'
]

{ #category : #'accessing-system' }
FLPharoPlatform >> absolutePathOf: aString [
	^ aString asFileReference fullName
]

{ #category : #'accessing-system' }
FLPharoPlatform >> imageName [
	^ Smalltalk image shortImageName
]

{ #category : #testing }
FLPharoPlatform >> isPharo [
	^ true
]

{ #category : #testing }
FLPharoPlatform >> isSpur [
	^ true
]

{ #category : #'accessing-compiler' }
FLPharoPlatform >> renamedMethodCopyOf: aMethod selector: aSymbol [
	| newMethod |
	newMethod := aMethod ast
		selector: aSymbol
		yourself.
	"#compiledMethod answers a new instance"
	^ newMethod compiledMethod
]

{ #category : #'accessing-system' }
FLPharoPlatform >> useAuthor: aString during: aBlock [
	(Smalltalk at: #Author)
		useAuthor: aString
		during: aBlock
]
