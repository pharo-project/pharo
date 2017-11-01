"
RGTraitDefinition is the concrete representation of a trait (no class)
"
Class {
	#name : #RGTraitDefinition,
	#superclass : #RGTraitDescriptionDefinition,
	#instVars : [
		'metaClass',
		'comment',
		'category',
		'package'
	],
	#category : 'Ring-Core-Kernel'
}

{ #category : #accessing }
RGTraitDefinition >> category [
	"Retrieves the tag-package associated to the receiver"

	^category
]

{ #category : #accessing }
RGTraitDefinition >> category: aSymbol [

	category:= aSymbol
]

{ #category : #accessing }
RGTraitDefinition >> classVariables [

	^#()
]

{ #category : #accessing }
RGTraitDefinition >> comment [
	"Retrieves the comment definition object associated to the receiver"
	
	^comment
]

{ #category : #accessing }
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

{ #category : #testing }
RGTraitDefinition >> hasComment [

	^comment isEmptyOrNil not
]

{ #category : #testing }
RGTraitDefinition >> hasMetaclass [

	^metaClass notNil
]

{ #category : #testing }
RGTraitDefinition >> hasStamp [

	^self stamp isEmptyOrNil not
]

{ #category : #accessing }
RGTraitDefinition >> instanceVariables [

	^#()
]

{ #category : #testing }
RGTraitDefinition >> isSameRevisionAs: aRGTraitDefinition [
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGTraitDefinition)
		and: [ self category = aRGTraitDefinition category 
		and: [ self traitCompositionSource = aRGTraitDefinition traitCompositionSource 
		and: [ ((self hasComment and: [ self comment isSameRevisionAs: aRGTraitDefinition comment ]) or:[ self hasComment not ])
		and: [ (self theMetaClass isSameRevisionAs: aRGTraitDefinition theMetaClass) ] ] ] ]
]

{ #category : #accessing }
RGTraitDefinition >> package [
	"Retrieves the package in which this trait is contained"
	
	^package
]

{ #category : #accessing }
RGTraitDefinition >> package: aRGPackage [
	"Sets the package in which this trait is contained"
	
	package:= aRGPackage
]

{ #category : #accessing }
RGTraitDefinition >> sharedPools [

	^#()
]

{ #category : #accessing }
RGTraitDefinition >> stamp [

	^self hasComment ifTrue:[ self comment stamp ] ifFalse:[ nil ]
]

{ #category : #accessing }
RGTraitDefinition >> stamp: aString [

	self hasComment
	ifTrue: [ self comment stamp: aString ]
	ifFalse:[ RGCommentDefinition new
						 parent: self; 
						 stamp: aString ]
]

{ #category : #accessing }
RGTraitDefinition >> theMetaClass [

	^metaClass
]

{ #category : #accessing }
RGTraitDefinition >> theNonMetaClass [

	^self
]

{ #category : #behavior }
RGTraitDefinition >> withMetaclass [
	"Registers explicitly the metaclass of a trait"

	metaClass:= RGMetatraitDefinition class: self.

]

{ #category : #behavior }
RGTraitDefinition >> withMetaclass: aRGMetatraitDefinition [
	"Registers explicitly the metaclass of a class"

	metaClass:= aRGMetatraitDefinition.
	metaClass baseClass: self.  

]
