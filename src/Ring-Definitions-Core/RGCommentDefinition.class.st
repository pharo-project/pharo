"
RGCommentDefinition is a first-class representation of class's comments
"
Class {
	#name : #RGCommentDefinition,
	#superclass : #RGElementDefinition,
	#instVars : [
		'content',
		'stamp'
	],
	#category : #'Ring-Definitions-Core-Base'
}

{ #category : #'instance creation' }
RGCommentDefinition class >> realClass: aClass [
	"Creates a ring comment definition from a Smalltalk class"
	^(super realClass: aClass)
		asActive;
		yourself
]

{ #category : #comparing }
RGCommentDefinition >> <= aRGCommentDefinition [
	"Sort comment definition according to: 1) name of the class"
	
	^(self parentName <= aRGCommentDefinition parentName)
]

{ #category : #'type of comments' }
RGCommentDefinition >> asActive [

	"Sets the receiver as active object, which will allow itself to retrieve its data from the class organization"
	self annotationNamed: self class statusKey put: #active.
]

{ #category : #'type of comments' }
RGCommentDefinition >> asHistorical [

	"Sets the receiver as historical object, which will allow itself to retrieve its data using the sourcePointer"
	| realClass |
	self annotationNamed: self class statusKey put: #historical.
	self sourcePointer ifNil:[
		realClass := self realClass.
		realClass notNil ifTrue: [ 
			realClass organization commentRemoteString
				ifNotNil: [:str | self sourcePointer: str sourcePointer ] ] ]
]

{ #category : #'type of comments' }
RGCommentDefinition >> asPassive [

	"Sets the receiver as passive object, which will allow itself to retrieve its data that was assigned in its creation"
	self annotationNamed: self class statusKey put: #passive
]

{ #category : #'stamp values' }
RGCommentDefinition >> author [ 

	^self annotationNamed: self class authorKey
		ifAbsentPut:[ self class parseAuthorAliasFrom: stamp ]
]

{ #category : #'stamp values' }
RGCommentDefinition >> author: aString [ 

	self annotationNamed: self class authorKey
			put: aString 
]

{ #category : #'to remove as soon as possible' }
RGCommentDefinition >> category [
	^self realClass category
]

{ #category : #'source pointers' }
RGCommentDefinition >> commentDataPointers [
	"Retrieves the combination key to look for information of the receiver in the source file"
	^'commentStamp:' -> #commentStamp:
]

{ #category : #accessing }
RGCommentDefinition >> content [

	self isActive 
		ifTrue: [ ^ self realClass organization classComment ].
	self isHistorical
		ifTrue: [ ^ self contentAtPointer ifNil:[ self realClass ifNil:[ content ] ifNotNil:[ :rc| rc organization classComment ] ] ].	
	^ content
]

{ #category : #accessing }
RGCommentDefinition >> content: anObject [

	content:= anObject
]

{ #category : #'source pointers' }
RGCommentDefinition >> contentAtPointer [
	"A RGCommentDefinition may be created to point the sourceFile in which case it retrieves the class comment"

	^ self sourcePointer notNil
		ifTrue: [ SourceFiles sourceCodeAt: self sourcePointer ]
		ifFalse:[ '' ]
]

{ #category : #'type of comments' }
RGCommentDefinition >> fromActiveToHistorical [
	"If the receiver was generated as an active comment, it can be converted to a historical one by reading the data of the real class (if exists)"
	
	self isActive ifTrue: [
		self asHistorical ]
]

{ #category : #'type of comments' }
RGCommentDefinition >> fromActiveToPassive [
	"If the receiver was generated as an active comment, it can be converted to a passive one by reading the data of the real class organization"

	| realClass |
	self isActive
		ifFalse: [ ^ self ].
	realClass := self realClass.
	realClass notNil
		ifTrue: [ 
			self content: realClass organization classComment.
			self stamp: realClass organization commentStamp ].
	self asPassive
]

{ #category : #accessing }
RGCommentDefinition >> fullName [
	"Keeps a unique description for the receiver. As annotation to avoid converting each time is invoked"
	
	^self annotationNamed: self class fullNameKey
		ifAbsentPut:[ (self parentName, ' ', self name) asSymbol ]
]

{ #category : #testing }
RGCommentDefinition >> hasAuthor [

	^self hasStamp and:[ self author isEmptyOrNil not ]
]

{ #category : #testing }
RGCommentDefinition >> hasStamp [

	^stamp isEmptyOrNil not

	
]

{ #category : #'type of comments' }
RGCommentDefinition >> isActive [

	"A ring comment isActive when it needs to access the class organization for retrieving its data"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ false ] 
		ifNotNil:[ :status| status == #active ]
]

{ #category : #testing }
RGCommentDefinition >> isComment [

	^true
]

{ #category : #testing }
RGCommentDefinition >> isEmptyOrNil [

	^content isEmptyOrNil 
]

{ #category : #testing }
RGCommentDefinition >> isFromTrait [
	^false
]

{ #category : #'type of comments' }
RGCommentDefinition >> isHistorical [

	"A ring comment can be used to point an old version of the receiver, in this case it will use the sourcePointer to retrieve its information"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ false ] 
		ifNotNil:[ :status| status == #historical ]
]

{ #category : #'type of comments' }
RGCommentDefinition >> isPassive [

	"A ring comment isPassive by default.  In this case it will retrieve the data that was assigned in its creation"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ true ] 
		ifNotNil:[ :status| status == #passive ]
]

{ #category : #testing }
RGCommentDefinition >> isSameRevisionAs: aRGCommentDefinition [
	"This method look for equality of the properties of the receiver"
	"A comment validates only its contents and not its stamp"
	
	^(super isSameRevisionAs: aRGCommentDefinition)
		and:[ self content = aRGCommentDefinition content ]
]

{ #category : #'to remove as soon as possible' }
RGCommentDefinition >> isValid [
	"for compatibility with method definition"

	^ true
]

{ #category : #'to remove as soon as possible' }
RGCommentDefinition >> methodClass [
	^ self realClass
]

{ #category : #accessing }
RGCommentDefinition >> name [

	^name ifNil:[ name := #Comment ]
]

{ #category : #printing }
RGCommentDefinition >> printOn: aStream [ 

	self parentName ifNotNil: [
		aStream nextPutAll: self parentName;
				  nextPutAll: ' ' ].
	aStream nextPutAll: self name
]

{ #category : #'to remove as soon as possible' }
RGCommentDefinition >> selector [
	^ self name
]

{ #category : #'backward compatibility' }
RGCommentDefinition >> sourceCode [

	^ self content   
]

{ #category : #'source pointers' }
RGCommentDefinition >> sourcePointer [
	"Retrieves the sourcePointer for this definition if exists"
	
	^self annotationNamed:  self class sourcePointerKey   
]

{ #category : #'source pointers' }
RGCommentDefinition >> sourcePointer: aNumber [

	self annotationNamed:  self class sourcePointerKey put: aNumber 
]

{ #category : #accessing }
RGCommentDefinition >> stamp [
	"Retrieves the user-alias + timestamp associated to the receiver (if exists)"
	
	self isActive 
		ifTrue: [ ^ self realClass organization commentStamp ].
	self isHistorical
		ifTrue: [ ^ self stampAtPointer ifNil:[ self realClass ifNil:[ stamp ] ifNotNil:[ :rc| rc organization commentStamp ] ] ].	
	^ stamp
]

{ #category : #accessing }
RGCommentDefinition >> stamp: anObject [

	stamp:= anObject
]

{ #category : #'source pointers' }
RGCommentDefinition >> stampAtPointer [
	"A RGMethodDefinition may be created to point the sourceFile in which case it retrieves the stamp"

	^ self sourcePointer notNil
		ifTrue: [ SourceFiles timeStampAt: self sourcePointer for: self commentDataPointers ]
		ifFalse:[ nil ]	
]

{ #category : #'stamp values' }
RGCommentDefinition >> timeStamp [ 

	^ self annotationNamed: self class timeStampKey 
		ifAbsentPut: [ self class 
							parseTimestampFrom: self stamp 
							default: (DateAndTime epoch) ]
]

{ #category : #'stamp values' }
RGCommentDefinition >> timeStamp: aTimestamp [ 

	self annotationNamed: self class timeStampKey
			put: aTimestamp 
]
