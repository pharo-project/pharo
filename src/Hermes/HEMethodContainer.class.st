Class {
	#name : #HEMethodContainer,
	#superclass : #HEAbstractExported,
	#instVars : [
		'methods',
		'classSideMethods',
		'traitComposition',
		'classTraitComposition',
		'category'
	],
	#category : 'Hermes-Model'
}

{ #category : #accessing }
HEMethodContainer >> category [
	^ category
]

{ #category : #accessing }
HEMethodContainer >> category: anObject [
	category := anObject
]

{ #category : #accessing }
HEMethodContainer >> classSideMethods [
	^ classSideMethods
]

{ #category : #accessing }
HEMethodContainer >> classSideMethods: anObject [
	classSideMethods := anObject
]

{ #category : #accessing }
HEMethodContainer >> classTraitComposition [
	^ classTraitComposition
]

{ #category : #accessing }
HEMethodContainer >> classTraitComposition: anObject [
	classTraitComposition := anObject
]

{ #category : #initialization }
HEMethodContainer >> doFromMethodContainer: aMethodContainer [
	category := aMethodContainer category.
	traitComposition := aMethodContainer traitComposition asExportedLiteral.
	classTraitComposition := aMethodContainer theMetaClass traitComposition
		asExportedLiteral.

	methods := aMethodContainer localMethods
		select: [ :e | e isExtension not ]
		thenCollect: [ :e | HEMethod for: e ].

	classSideMethods := aMethodContainer theMetaClass localMethods
		select: [ :e | e isExtension not ]
		thenCollect: [ :e | HEMethod for: e ]
]

{ #category : #reading }
HEMethodContainer >> doReadHeaderFrom: aReader [
	self subclassResponsibility
]

{ #category : #reading }
HEMethodContainer >> doReadMethodsFrom: aReader [
	| numberOfMethods numberOfClassMethods |
	category := aReader readByteSymbol.
	numberOfMethods := aReader readInt32.

	methods := (1 to: numberOfMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.
	numberOfClassMethods := aReader readInt32.

	classSideMethods := (1 to: numberOfClassMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.
	traitComposition := HEExportedLiteral readFrom: aReader.

	classTraitComposition := HEExportedLiteral readFrom: aReader
]

{ #category : #writing }
HEMethodContainer >> doWriteHeaderInto: aWriter [ 
	self subclassResponsibility
]

{ #category : #writing }
HEMethodContainer >> doWriteMethods: aWriter [
	aWriter writeByteString: category.

	aWriter writeInt32: methods size.
	methods do: [ :e | e writeInto: aWriter ].

	aWriter writeInt32: classSideMethods size.
	classSideMethods do: [ :e | e writeInto: aWriter ].

	traitComposition writeInto: aWriter.
	classTraitComposition writeInto: aWriter
]

{ #category : #accessing }
HEMethodContainer >> methods [
	^ methods
]

{ #category : #accessing }
HEMethodContainer >> methods: anObject [
	methods := anObject
]

{ #category : #reading }
HEMethodContainer >> readFrom: aReader [
	self doReadHeaderFrom: aReader.
	self doReadMethodsFrom: aReader
]

{ #category : #accessing }
HEMethodContainer >> traitComposition [
	^ traitComposition
]

{ #category : #accessing }
HEMethodContainer >> traitComposition: anObject [
	traitComposition := anObject
]

{ #category : #writing }
HEMethodContainer >> writeInto: aWriter [
	self doWriteHeaderInto: aWriter.
	self doWriteMethods: aWriter
]
