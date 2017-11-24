"
RGMetatraitDefinition is a concrete representation of metaclasses of traits
"
Class {
	#name : #RGMetatraitDefinition,
	#superclass : #RGTraitDescriptionDefinition,
	#instVars : [
		'baseClass'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'class initialization' }
RGMetatraitDefinition class >> class: anORTraitDefinition [

	^self new
		baseClass: anORTraitDefinition;
		yourself
]

{ #category : #accessing }
RGMetatraitDefinition >> baseClass: anRGTraitDefinition [

	baseClass:= anRGTraitDefinition.
	self name: (baseClass name, ' classTrait') asSymbol
]

{ #category : #accessing }
RGMetatraitDefinition >> category [
	^ self theNonMetaClass category
]

{ #category : #accessing }
RGMetatraitDefinition >> environment [

	^baseClass environment
]

{ #category : #testing }
RGMetatraitDefinition >> isMeta [

	^true
]

{ #category : #testing }
RGMetatraitDefinition >> isSameRevisionAs: aRGMetatraitDefinition [
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGMetatraitDefinition)
		and: [ self traitCompositionSource = aRGMetatraitDefinition traitCompositionSource ]
]

{ #category : #accessing }
RGMetatraitDefinition >> package [

	^baseClass package
]

{ #category : #accessing }
RGMetatraitDefinition >> realClass [

	^baseClass realClass theMetaClass
]

{ #category : #accessing }
RGMetatraitDefinition >> theMetaClass [

	^self
]

{ #category : #accessing }
RGMetatraitDefinition >> theNonMetaClass [

	^baseClass
]
