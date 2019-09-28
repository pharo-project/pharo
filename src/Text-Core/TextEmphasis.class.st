"
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	itallic
	4	underlined
	8	narrow
	16	struck out
"
Class {
	#name : #TextEmphasis,
	#superclass : #TextAttribute,
	#instVars : [
		'emphasisCode',
		'setMode'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #constants }
TextEmphasis class >> allBits [
	"all of the bits use in encoding my Instances"

	^2r11111
]

{ #category : #'instance creation' }
TextEmphasis class >> bold [
	^ self new emphasisCode: 1
]

{ #category : #'instance creation' }
TextEmphasis class >> fromCode: anIntegerBitMask [
	^ self new emphasisCode: (anIntegerBitMask bitAnd: self allBits)
]

{ #category : #'instance creation' }
TextEmphasis class >> italic [
	^ self new emphasisCode: 2
]

{ #category : #'instance creation' }
TextEmphasis class >> narrow [
	^ self new emphasisCode: 8
]

{ #category : #'instance creation' }
TextEmphasis class >> normal [
	^ self new emphasisCode: 0
]

{ #category : #'instance creation' }
TextEmphasis class >> struckOut [
	^ self new emphasisCode: 16
]

{ #category : #'instance creation' }
TextEmphasis class >> underlined [
	^ self new emphasisCode: 4
]

{ #category : #comparing }
TextEmphasis >> = other [ 
	^ other value = self value
]

{ #category : #combining }
TextEmphasis >> add: anotherCodeOrEmphasis [
	"add anotherCodeOrEmphasis to me"
	emphasisCode := emphasisCode bitOr: anotherCodeOrEmphasis value
]

{ #category : #private }
TextEmphasis >> allBits [
	^ self class allBits
]

{ #category : #testing }
TextEmphasis >> dominatedByCmd0 [
	"Cmd-0 should turn off emphasis"
	^ true
]

{ #category : #accessing }
TextEmphasis >> dominates: other [
	(emphasisCode = 0 and: [other dominatedByCmd0]) ifTrue: [^ true].
	^ (other class == self class)
		and: [emphasisCode = other emphasisCode]
]

{ #category : #accessing }
TextEmphasis >> emphasisCode [
	^ emphasisCode
]

{ #category : #accessing }
TextEmphasis >> emphasisCode: int [
	emphasisCode := int.
	setMode := true
]

{ #category : #comparing }
TextEmphasis >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^emphasisCode hash

]

{ #category : #printing }
TextEmphasis >> printOn: strm [
	super printOn: strm.
	strm nextPutAll: ' code: '; print: emphasisCode
]

{ #category : #combining }
TextEmphasis >> remove: anotherCodeOrEmphasis [
	"remove anotherCodeOrEmphasis from me"
	emphasisCode := emphasisCode bitAnd: 
		((anotherCodeOrEmphasis value bitInvert) bitAnd: (self allBits))
]

{ #category : #accessing }
TextEmphasis >> set [
	^ setMode and: [emphasisCode ~= 0]
]

{ #category : #accessing }
TextEmphasis >> turnOff [
	setMode := false
]

{ #category : #accessing }
TextEmphasis >> value [
	"a synonym for empahasisCode, to make me polymorphic with Numbers"
	
	^ emphasisCode
]
