"
RGTraitDefinition is the concrete representation of a trait (no class)
"
Class {
	#name : 'RGTraitDefinition',
	#superclass : 'RGTraitDescriptionDefinition',
	#instVars : [
		'metaClass',
		'comment',
		'package',
		'packageTag'
	],
	#category : 'Ring-Definitions-Core-Base',
	#package : 'Ring-Definitions-Core',
	#tag : 'Base'
}

{ #category : 'accessing - parallel hierarchy' }
RGTraitDefinition >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	^ metaClass
]

{ #category : 'accessing' }
RGTraitDefinition >> classVariables [

	^#()
]

{ #category : 'accessing' }
RGTraitDefinition >> comment [
	"Retrieves the comment definition object associated to the receiver"

	^comment
]

{ #category : 'accessing' }
RGTraitDefinition >> comment: anObject [
	"Sets a RGCommentDefinition object based on the argument"

	comment := anObject isRingObject
		ifFalse: [
			RGCommentDefinition new
				parent: self;
				content: anObject;
				yourself ]
		ifTrue: [anObject ]
]

{ #category : 'testing' }
RGTraitDefinition >> hasComment [

	^comment isEmptyOrNil not
]

{ #category : 'testing' }
RGTraitDefinition >> hasMetaclass [

	^metaClass isNotNil
]

{ #category : 'testing' }
RGTraitDefinition >> hasStamp [

	^self stamp isEmptyOrNil not
]

{ #category : 'accessing' }
RGTraitDefinition >> inheritsFrom: aClass [

	^ self realClass inheritsFrom: aClass
]

{ #category : 'accessing - parallel hierarchy' }
RGTraitDefinition >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	^ self
]

{ #category : 'accessing' }
RGTraitDefinition >> instanceVariables [

	^#()
]

{ #category : 'testing' }
RGTraitDefinition >> isSameRevisionAs: aRGTraitDefinition [
	"This method look for equality of the properties of the receiver"

	^ (super isSameRevisionAs: aRGTraitDefinition) and: [
		  self package name = aRGTraitDefinition package name and: [
			  self packageTag = aRGTraitDefinition packageTag and: [
				  self traitCompositionSource = aRGTraitDefinition traitCompositionSource and: [
					  ((self hasComment and: [ self comment isSameRevisionAs: aRGTraitDefinition comment ]) or: [ self hasComment not ]) and: [
						  self classSide isSameRevisionAs: aRGTraitDefinition classSide ] ] ] ] ]
]

{ #category : 'accessing' }
RGTraitDefinition >> package [
	"Retrieves the package in which this trait is contained"

	^package
]

{ #category : 'accessing' }
RGTraitDefinition >> package: aRGPackage [
	"Sets the package in which this trait is contained"

	package:= aRGPackage
]

{ #category : 'accessing' }
RGTraitDefinition >> packageTag [

	^ packageTag
]

{ #category : 'accessing' }
RGTraitDefinition >> packageTag: anObject [

	packageTag := anObject
]

{ #category : 'accessing' }
RGTraitDefinition >> sharedPools [

	^#()
]

{ #category : 'accessing' }
RGTraitDefinition >> stamp [

	^self hasComment ifTrue:[ self comment stamp ] ifFalse:[ nil ]
]

{ #category : 'accessing' }
RGTraitDefinition >> stamp: aString [

	self hasComment
	ifTrue: [ self comment stamp: aString ]
	ifFalse:[ RGCommentDefinition new
						 parent: self;
						 stamp: aString ]
]

{ #category : 'behavior' }
RGTraitDefinition >> withMetaclass [
	"Registers explicitly the metaclass of a trait"

	metaClass:= RGMetatraitDefinition class: self
]

{ #category : 'behavior' }
RGTraitDefinition >> withMetaclass: aRGMetatraitDefinition [
	"Registers explicitly the metaclass of a class"

	metaClass:= aRGMetatraitDefinition.
	metaClass baseClass: self
]
