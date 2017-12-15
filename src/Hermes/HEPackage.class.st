Class {
	#name : #HEPackage,
	#superclass : #HEAbstractExported,
	#instVars : [
		'version',
		'packageName',
		'classes',
		'extensionMethods',
		'traits'
	],
	#category : #'Hermes-Model'
}

{ #category : #formatting }
HEPackage class >> formatVersion [
	^ 1
]

{ #category : #adding }
HEPackage >> addClass: anExportedClass [
	classes add: anExportedClass.
]

{ #category : #accessing }
HEPackage >> classes [
	^ classes
]

{ #category : #accessing }
HEPackage >> classes: anObject [
	classes := anObject
]

{ #category : #accessing }
HEPackage >> extensionMethods [
	^ extensionMethods
]

{ #category : #accessing }
HEPackage >> extensionMethods: anObject [
	extensionMethods := anObject
]

{ #category : #initialization }
HEPackage >> initialize [
	version := self class formatVersion.
	classes := OrderedCollection new.
	traits := OrderedCollection new.

	extensionMethods := OrderedCollection new.
]

{ #category : #accessing }
HEPackage >> packageName [
	^ packageName
]

{ #category : #accessing }
HEPackage >> packageName: anObject [
	packageName := anObject
]

{ #category : #printing }
HEPackage >> printOn: aStream [
	aStream
		nextPutAll: 'HEPackage ( ';
		nextPutAll: packageName asString;
		nextPutAll: ' )'
]

{ #category : #reading }
HEPackage >> readClassFrom: aReader [
	classes add: (HEClass readFrom: aReader).
	
]

{ #category : #reading }
HEPackage >> readFrom: aReader [
	| numberOfTraits numberOfClasses numberOfExtensionMethods |
	version := aReader readInt32.
	
	version = self class formatVersion ifFalse:[self error:'Invalid Hermes file version, expecting: ' , self class formatVersion asString , ' but was: ' , version asString].
	
	packageName := aReader readByteSymbol.

	numberOfTraits := aReader readInt32.
	1 to: numberOfTraits do: [ :idx | traits add: (HETrait readFrom: aReader)].

	numberOfClasses := aReader readInt32.
	1 to: numberOfClasses do: [ :idx | classes add: (HEClass readFrom: aReader)].

	numberOfExtensionMethods := aReader readInt32.
	1 to: numberOfExtensionMethods do: [ :idx | extensionMethods add: (HEMethod readFrom: aReader) ]  
]

{ #category : #accessing }
HEPackage >> traits [
	^ traits
]

{ #category : #accessing }
HEPackage >> traits: anObject [
	traits := anObject
]

{ #category : #accessing }
HEPackage >> version [
	^ version
]
