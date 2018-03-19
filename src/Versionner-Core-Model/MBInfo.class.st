"
Common superclass for Versionner infos
"
Class {
	#name : #MBInfo,
	#superclass : #Object,
	#instVars : [
		'name',
		'text',
		'validationResult',
		'validationIcon',
		'validationIconSymbol'
	],
	#classVars : [
		'ValidateAll'
	],
	#category : #'Versionner-Core-Model'
}

{ #category : #accessing }
MBInfo class >> helpLabel [

	self subclassResponsibility
]

{ #category : #accessing }
MBInfo class >> validateAll [

	ValidateAll == nil ifTrue: [ ValidateAll := false ].
	^ValidateAll
]

{ #category : #accessing }
MBInfo class >> validateAll: aBool [

	ValidateAll := aBool
]

{ #category : #accessing }
MBInfo >> announcer [ 

	^self configurationRoot announcer
]

{ #category : #converting }
MBInfo >> asStringOrText [

	^self text
]

{ #category : #accessing }
MBInfo >> browseValidationResults [

	self validate inspect
]

{ #category : #accessing }
MBInfo >> buildStringOrText [
	^self printString
]

{ #category : #actions }
MBInfo >> clearValidationIcon [

	validationIcon := nil
]

{ #category : #commands }
MBInfo >> commands [
	^ (self class allSelectors select:
		[ :k | k beginsWith: 'cmd'  ]) collect: [ :k | self perform: k ]
]

{ #category : #accessing }
MBInfo >> configurationInfo [

	^self subclassResponsibility 
]

{ #category : #accessing }
MBInfo >> configurationRoot [

	^self configurationInfo configurationRoot
]

{ #category : #utilities }
MBInfo >> extractPackageSignatureFrom: aWorkingCopy [
	| ref |
	(aWorkingCopy isNil or: [ aWorkingCopy ancestors isEmpty ])
		ifTrue: [ ^ '' ].
	ref := GoferVersionReference name: aWorkingCopy ancestors first name.
	^ ref author , '.' , ref versionNumber asString
]

{ #category : #testing }
MBInfo >> interestedInPackageNamed: packageName [

	^false
]

{ #category : #testing }
MBInfo >> isDirty [
	^ false
]

{ #category : #accessing }
MBInfo >> name [
	^ name
]

{ #category : #accessing }
MBInfo >> name: aName [
	name := aName
]

{ #category : #printing }
MBInfo >> printOn: aStream [
	self name ifNil: [ aStream nextPutAll: '' . ^ self ].
	aStream nextPutAll: self name
]

{ #category : #accessing }
MBInfo >> project [
	"Answer the MetacelloProject associated with the receiver."
	
	^nil
]

{ #category : #utilities }
MBInfo >> repositories [
	"Return the repositories associated with the receiver."

	^#()
]

{ #category : #accessing }
MBInfo >> text [
	text ifNil: [ text := self buildStringOrText ].
	^text
]

{ #category : #accessing }
MBInfo >> text: anObject [
	text := anObject
]

{ #category : #accessing }
MBInfo >> validate [
	"Returns by default an empty collection of problems"
	
	^#()
]

{ #category : #actions }
MBInfo >> validateFull [

	self  validationResult: nil.
	^self validate
]

{ #category : #accessing }
MBInfo >> validationResult: anObject [

	validationResult := anObject
]
