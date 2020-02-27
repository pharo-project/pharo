"
RGMetatraitDefinition is a concrete representation of metaclasses of traits
"
Class {
	#name : #RGMetatraitDefinition,
	#superclass : #RGTraitDescriptionDefinition,
	#instVars : [
		'baseClass'
	],
	#category : #'Ring-Definitions-Core-Base'
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
	^ self instanceSide category
]

{ #category : #'accessing - parallel hierarchy' }
RGMetatraitDefinition >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	^ self
]

{ #category : #accessing }
RGMetatraitDefinition >> environment [

	^baseClass environment
]

{ #category : #'accessing - parallel hierarchy' }
RGMetatraitDefinition >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	^ baseClass
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

	^baseClass realClass classSide
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGMetatraitDefinition >> theMetaClass [

	^self
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGMetatraitDefinition >> theNonMetaClass [

	^baseClass
]
