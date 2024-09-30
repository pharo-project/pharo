"
Abstract font family
"
Class {
	#name : #AbstractFontFamily,
	#superclass : #Object,
	#instVars : [
		#familyName,
		#members
	],
	#category : #'Fonts-Infrastructure-Base'
}

{ #category : #testing }
AbstractFontFamily class >> isAbstract [

	 ^self name = #AbstractFontFamily
]

{ #category : #'member lookup' }
AbstractFontFamily >> closestMemberWithStretchValue: stretchValue weightValue: weightValue slantValue: slantValue [

	"answer the member that has weight, slant and stretch values that most closely
	match those given by stretchValue, weightValue, and slantValue"

	^ (self members asSortedCollection: [ :a :b |
		   a
			   isCloserMatchThan: b
			   toStretch: stretchValue
			   weight: weightValue
			   slant: slantValue ]) first
]

{ #category : #accessing }
AbstractFontFamily >> familyName [
	"Answer the value of familyName"

	^ familyName
]

{ #category : #accessing }
AbstractFontFamily >> familyName: anObject [
	"Set the value of familyName"

	familyName := anObject
]

{ #category : #accessing }
AbstractFontFamily >> members [
	"Answer the value of members"

	^ members
]

{ #category : #printing }
AbstractFontFamily >> printOn: aStream [
	aStream
		print: self class name;
		nextPut: $ ;
		print: self familyName
]
