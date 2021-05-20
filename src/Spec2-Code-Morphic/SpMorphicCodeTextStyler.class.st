Class {
	#name : #SpMorphicCodeTextStyler,
	#superclass : #SHRBTextStyler,
	#instVars : [
		'attributeTable'
	],
	#category : #'Spec2-Code-Morphic'
}

{ #category : #'instance creation' }
SpMorphicCodeTextStyler class >> new [

	^ self newWithAttributeTable: self styleTable
]

{ #category : #'instance creation' }
SpMorphicCodeTextStyler class >> newWithAttributeTable: attrTable [

	^ self basicNew
		initializeAttributeTable: attrTable;
		yourself
]

{ #category : #'instance creation' }
SpMorphicCodeTextStyler class >> newWithAttributeTableNamed: aString [

	^ self newWithAttributeTable: (self newAttributesForStyleTableNamed: aString)
]

{ #category : #private }
SpMorphicCodeTextStyler >> attributesFor: aSymbol [ 
	
	^ attributeTable at: aSymbol ifAbsent: [ #() ]
]

{ #category : #initialization }
SpMorphicCodeTextStyler >> initializeAttributeTable: attrTable [

	attributeTable := attrTable.
	self initialize
]
