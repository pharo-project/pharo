"
RG2CommentDefinition is a first-class representation of class's comments
"
Class {
	#name : #RG2CommentDefinition,
	#superclass : #RG2ElementDefinition,
	#instVars : [
		'content',
		'author',
		'time'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'managing container' }
RG2CommentDefinition >> addoptToParentStub [

	super addoptToParentStub.

	self environment backend createUnresolvedClassGroupFor: self parent
]

{ #category : #'accessing - backend' }
RG2CommentDefinition >> author [

	^ self backend forBehavior classCommentAuthorFor: self
]

{ #category : #'accessing - backend' }
RG2CommentDefinition >> author: aString [

	self backend forBehavior setClassCommentAuthorFor: self to: aString.
	
	self announcer behaviorCommentModified: self parent.
]

{ #category : #'accessing - backend' }
RG2CommentDefinition >> content [

	^ self backend forBehavior classCommentContentFor: self
]

{ #category : #'accessing - backend' }
RG2CommentDefinition >> content: aString [

	self backend forBehavior setClassCommentContentFor: self to: aString.
	
	self announcer behaviorCommentModified: self parent.
]

{ #category : #'default model values' }
RG2CommentDefinition >> defaultAuthor [

	^ ''
]

{ #category : #'default model values' }
RG2CommentDefinition >> defaultContent [

	^ ''
]

{ #category : #'managing container' }
RG2CommentDefinition >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #'default model values' }
RG2CommentDefinition >> defaultTime [

	^ DateAndTime new
]

{ #category : #accessing }
RG2CommentDefinition >> fullName [
	
	^ (self parent name, ' comment') asSymbol
]

{ #category : #initialization }
RG2CommentDefinition >> initialize [

	super initialize.
	
	content := self unresolvedValue: self defaultContent.	
	author := self unresolvedValue: self defaultAuthor.	
	time := self unresolvedValue: self defaultTime.	
	
]

{ #category : #initialization }
RG2CommentDefinition >> initializeUnresolved [

	super initializeUnresolved.
	
	content := self unresolvedValue: self defaultContent.	
	author := self unresolvedValue: self defaultAuthor.	
	time := self unresolvedValue: self defaultTime.	
	
]

{ #category : #testing }
RG2CommentDefinition >> isComment [

	^ true
]

{ #category : #testing }
RG2CommentDefinition >> isEmptyOrNil [

	^ self content isEmptyOrNil 
]

{ #category : #resolving }
RG2CommentDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	super makeMinimallyResolved.
	
	content := self content.
	author := self author.
	time := self time.
]

{ #category : #resolving }
RG2CommentDefinition >> makeResolved [

	super makeResolved.
	
	content := self content.
	author := self author.
	time := self time.
]

{ #category : #printing }
RG2CommentDefinition >> printOn: aStream [ 

	self parent name ifNotNil: [
		aStream nextPutAll: self parent name;
				  nextPutAll: ' ' ].
	aStream nextPutAll: self name asString
]

{ #category : #'private - backend interface' }
RG2CommentDefinition >> pvtAuthor [

	^ author value
]

{ #category : #'private - backend interface' }
RG2CommentDefinition >> pvtAuthor: aString [

	^ author := aString
]

{ #category : #'private - backend interface' }
RG2CommentDefinition >> pvtContent [

	^ content value
]

{ #category : #'private - backend interface' }
RG2CommentDefinition >> pvtContent: aString [

	^ content := aString
]

{ #category : #'private - backend interface' }
RG2CommentDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#content -> content. 
		#author -> author. 
		#time -> time. 
	}
	
	
]

{ #category : #'private - backend interface' }
RG2CommentDefinition >> pvtTime [

	^ time value
]

{ #category : #'private - backend interface' }
RG2CommentDefinition >> pvtTime: aDateAndTime [

	^ time := aDateAndTime
]

{ #category : #'backward compatibility' }
RG2CommentDefinition >> sourceCode [

	^ self content   
]

{ #category : #'accessing - backend' }
RG2CommentDefinition >> time [

	^ self backend forBehavior classCommentTimeFor: self
]

{ #category : #'accessing - backend' }
RG2CommentDefinition >> time: aDateAndTime [

	self backend forBehavior setClassCommentTimeFor: self to: aDateAndTime.
	
	self announcer behaviorCommentModified: self parent.
]
