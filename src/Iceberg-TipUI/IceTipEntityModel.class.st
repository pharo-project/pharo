"
I'm a generic model for Iceberg entities (tipically, repositories, commits, packages, etc., etc., etc.) 
"
Class {
	#name : #IceTipEntityModel,
	#superclass : #IceTipModel,
	#instVars : [
		'entity',
		'repositoryModel'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #'instance creation' }
IceTipEntityModel class >> new [
	self error: 'Use #for:'
]

{ #category : #'instance creation' }
IceTipEntityModel class >> on: anObject [ 
	^ self basicNew 
		initializeEntity: anObject;
		yourself
]

{ #category : #'instance creation' }
IceTipEntityModel class >> repositoryModel: aRepositoryModel on: anObject [ 
	^ self basicNew		
		initializeEntity: anObject;
		repositoryModel: aRepositoryModel;
		yourself
]

{ #category : #TOMOVE }
IceTipEntityModel >> colorError [
	^ Smalltalk ui theme dangerTextColor
]

{ #category : #TOMOVE }
IceTipEntityModel >> colorModified [
	self flag: #todo. "Make themable"
	^ Color r: 0 g: 0.5 b: 0
]

{ #category : #TOMOVE }
IceTipEntityModel >> colorNotLoaded [
	^ Smalltalk ui theme disabledTextColor
]

{ #category : #TOMOVE }
IceTipEntityModel >> colorWarning [
	^ Smalltalk ui theme warningTextColor
]

{ #category : #tagging }
IceTipEntityModel >> createTag: aString thenDo: aBlockClosure [ 
	<noCache>

	IceTipStandardAction new 
		repository: self entity;
		message: ('Creating tag {1} on commit {2}' format: { aString. self commit shortId });
		onSuccessRepositoryModified;
		execute: [ | branch |
			branch := self commit createTag: aString.
			aBlockClosure value: branch ]
	
]

{ #category : #accessing }
IceTipEntityModel >> descriptionWithDecoration [
	"A decorated description of this entity. 
	 It can return a text (in case decorations apply) or a simple string."
	^ self subclassResponsibility
]

{ #category : #accessing }
IceTipEntityModel >> displayString [
	^ self entity name
]

{ #category : #accessing }
IceTipEntityModel >> entity [
	^ entity
]

{ #category : #initialization }
IceTipEntityModel >> initializeEntity: anObject [
	entity := anObject.
	self initialize
	
]

{ #category : #testing }
IceTipEntityModel >> isModelOf: anObject [
	^ self entity = anObject
]

{ #category : #accessing }
IceTipEntityModel >> repositoryModel [
	^ repositoryModel
]

{ #category : #accessing }
IceTipEntityModel >> repositoryModel: anObject [
	repositoryModel := anObject
]

{ #category : #TOMOVE }
IceTipEntityModel >> toWarningMessage: aString [
	^ aString asText allBold makeAllColor: self colorWarning
]
