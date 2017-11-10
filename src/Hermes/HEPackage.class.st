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
	#category : #Hermes-Model
}

{ #category : #formatting }
HEPackage class >> for: aPackage [
	^ self new
		fromPackage: aPackage;
		yourself
]

{ #category : #formatting }
HEPackage class >> formatVersion [
	^ 1
]

{ #category : #adding }
HEPackage >> addClass: anExportedClass [
	classes add: anExportedClass.
]

{ #category : #testing }
HEPackage >> allSharedPoolsOf: aClass in: aCollection [
	^ ({aClass} , (aClass allSuperclassesIn: aCollection)) flatCollect: [:e | e sharedPoolsIn: aCollection].
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
HEPackage >> fromPackage: aPackage [
	| allClasses|
	packageName := aPackage name.

	traits := (aPackage definedClasses select: #isTrait) collect: [ :e | HETrait for: e ] as: OrderedCollection.	
	allClasses := (aPackage definedClasses reject: #isTrait).
	(aPackage definedClasses reject: #isTrait) do: [ :e | self addClass: (HEClass for: e) ].

	aPackage extensionMethods do: [ :e | extensionMethods add: (HEMethod for: e) ]
]

{ #category : #initialization }
HEPackage >> initialize [
	version := self class formatVersion.
	classes := OrderedCollection new.
	traits := OrderedCollection new.

	extensionMethods := OrderedCollection new.
]

{ #category : #testing }
HEPackage >> numberOfRequiredClassesOf: aClass in: aCollection [
	^ ((aClass allSuperclassesIn: aCollection) , ((self allSharedPoolsOf: aClass in: aCollection) flatCollect: [:e | {e} , (e allSuperclassesIn: aCollection)])) size
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

{ #category : #adding }
HEPackage >> sortContent [
	| allClasses|
	traits := traits sorted: [ :a :b | (a traitComposition allDependenciesIn: traits) size < (b traitComposition allDependenciesIn: traits) size ].
	
	allClasses := classes.
	classes := classes sorted: [ :a :b | (self numberOfRequiredClassesOf: a in: allClasses) < (self numberOfRequiredClassesOf: b in: allClasses) ].

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

{ #category : #writing }
HEPackage >> writeInto: aWriter [
	self sortContent.

	aWriter writeInt32: version.
	aWriter writeByteString: packageName.

	aWriter writeInt32: traits size.
	traits do: [ :each | each writeInto: aWriter ].

	aWriter writeInt32: classes size.
	classes do: [ :each | each writeInto: aWriter ].

	aWriter writeInt32: extensionMethods size.
	extensionMethods do: [ :e | e writeInto: aWriter ].

	aWriter flush.
]
