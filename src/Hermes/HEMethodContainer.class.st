"
I implement the common behavior between classes and traits.
I implement all the handling of methods (instance side and class side) and the trait compositions (instance side and class side).
The headers of a class and a trait are different. Each subclass implements this reading.
"
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
	#category : #'Hermes-Model'
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

{ #category : #reading }
HEMethodContainer >> doReadHeaderFrom: aReader [
	"The headers of a class and a trait are different. Each subclass implements this reading"
	self subclassResponsibility
]

{ #category : #reading }
HEMethodContainer >> doReadMethodsFrom: aReader [
	| numberOfMethods numberOfClassMethods |
	"The category of a trait or a class is stored as a byteString."
	category := aReader readByteSymbol.
	"The methods are stored with the quantity before them. First the instance side and the the class side."
	numberOfMethods := aReader readInt32.

	methods := (1 to: numberOfMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.
	numberOfClassMethods := aReader readInt32.

	classSideMethods := (1 to: numberOfClassMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.

	"The trait composition are readed after."
	traitComposition := HEExportedLiteral readFrom: aReader.
	classTraitComposition := HEExportedLiteral readFrom: aReader
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
