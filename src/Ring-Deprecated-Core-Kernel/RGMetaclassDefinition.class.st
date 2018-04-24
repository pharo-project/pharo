"
RGMetaclassDefinition is a concrete representation of metaclasses of classes (traits are excluded)
"
Class {
	#name : #RGMetaclassDefinition,
	#superclass : #RGClassDescriptionDefinition,
	#instVars : [
		'baseClass'
	],
	#category : #'Ring-Deprecated-Core-Kernel'
}

{ #category : #'class initialization' }
RGMetaclassDefinition class >> class: anORClassDefinition [

	^self new
		baseClass: anORClassDefinition;
		yourself
]

{ #category : #variables }
RGMetaclassDefinition >> allClassVarNames [

	^self instanceSide allClassVarNames
]

{ #category : #variables }
RGMetaclassDefinition >> allSharedPoolNames [

	^self theNonMetaClass allSharedPoolNames
]

{ #category : #accessing }
RGMetaclassDefinition >> baseClass: aRGClassDefinition [

	baseClass:= aRGClassDefinition.
	self name: (baseClass name, ' class') asSymbol
]

{ #category : #accessing }
RGMetaclassDefinition >> category [
	^ self instanceSide category
]

{ #category : #'accessing - parallel hierarchy' }
RGMetaclassDefinition >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	^ self
]

{ #category : #accessing }
RGMetaclassDefinition >> environment [

	^baseClass environment
]

{ #category : #accessing }
RGMetaclassDefinition >> inheritsFrom: aClass [

	^ self realClass inheritsFrom: aClass
]

{ #category : #'accessing - parallel hierarchy' }
RGMetaclassDefinition >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	^ baseClass
]

{ #category : #testing }
RGMetaclassDefinition >> isMeta [

	^true
]

{ #category : #testing }
RGMetaclassDefinition >> isSameRevisionAs: aRGMetaclassDefinition [
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGMetaclassDefinition)
		and:[ self traitCompositionSource = aRGMetaclassDefinition traitCompositionSource ]
]

{ #category : #accessing }
RGMetaclassDefinition >> package [

	^baseClass package
]

{ #category : #accessing }
RGMetaclassDefinition >> realClass [

	^baseClass realClass classSide
]

{ #category : #printing }
RGMetaclassDefinition >> storeOn: aStream [
	
	aStream nextPut: $(. 
	super storeOn: aStream.
	aStream nextPutAll: ' baseClass:'.
	self instanceSide storeOn: aStream.
	aStream nextPut: $).
	

]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGMetaclassDefinition >> theMetaClass [

	^self
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGMetaclassDefinition >> theNonMetaClass [

	^baseClass
]
