"
Abstract font family
"
Class {
	#name : #FontFamilyAbstract,
	#superclass : #Object,
	#instVars : [
		'familyName',
		'members'
	],
	#category : 'FontInfrastructure'
}

{ #category : #'member lookup' }
FontFamilyAbstract >> closestMemberWithStretchValue: stretchValue weightValue: weightValue slantValue: slantValue [
	"answer the member that has weight, slant and stretch values that most closely 
	match those given by stretchValue, weightValue, and slantValue"
	
	^(self members asSortedCollection:[:a :b | 
		a isCloserMatchThan: b 
			toStretch: stretchValue 
			weight: weightValue 
			slant: slantValue]) first. 
]

{ #category : #accessing }
FontFamilyAbstract >> familyName [
	"Answer the value of familyName"

	^ familyName
]

{ #category : #accessing }
FontFamilyAbstract >> familyName: anObject [
	"Set the value of familyName"

	familyName := anObject
]

{ #category : #accessing }
FontFamilyAbstract >> members [
	"Answer the value of members"

	^ members
]

{ #category : #printing }
FontFamilyAbstract >> printOn: aStream [
	aStream 
		nextPutAll: self class name asString;
		nextPut: $ ;
		nextPutAll: self familyName printString
]
