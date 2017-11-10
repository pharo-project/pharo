"
RGMetaclassDefinition is a concrete representation of metaclasses of classes (traits are excluded)
"
Class {
	#name : #RGMetaclassDefinition,
	#superclass : #RGClassDescriptionDefinition,
	#instVars : [
		'baseClass'
	],
	#category : #Ring-Core-Kernel
}

{ #category : #'class initialization' }
RGMetaclassDefinition class >> class: anORClassDefinition [

	^self new
		baseClass: anORClassDefinition;
		yourself
]

{ #category : #variables }
RGMetaclassDefinition >> allClassVarNames [

	^self theNonMetaClass allClassVarNames
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
	^ self theNonMetaClass category
]

{ #category : #accessing }
RGMetaclassDefinition >> environment [

	^baseClass environment
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

	^baseClass realClass theMetaClass
]

{ #category : #printing }
RGMetaclassDefinition >> storeOn: aStream [
	
	aStream nextPut: $(. 
	super storeOn: aStream.
	aStream nextPutAll: ' baseClass:'.
	self theNonMetaClass storeOn: aStream.
	aStream nextPut: $).
	

]

{ #category : #accessing }
RGMetaclassDefinition >> theMetaClass [

	^self
]

{ #category : #accessing }
RGMetaclassDefinition >> theNonMetaClass [

	^baseClass
]
