"
RGComment is a first-class representation of class's comments
"
Class {
	#name : 'RGComment',
	#superclass : 'RGElement',
	#instVars : [
		'content',
		'author',
		'time'
	],
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'managing container' }
RGComment >> addoptToParentStub [

	super addoptToParentStub.

	self environment backend createUnresolvedClassGroupFor: self parent
]

{ #category : 'accessing - backend' }
RGComment >> author [

	^ self backend forBehavior classCommentAuthorFor: self
]

{ #category : 'accessing - backend' }
RGComment >> author: aString [

	self backend forBehavior setClassCommentAuthorFor: self to: aString.

	self announcer behaviorCommentModified: self parent
]

{ #category : 'accessing - backend' }
RGComment >> content [

	^ self backend forBehavior classCommentContentFor: self
]

{ #category : 'accessing - backend' }
RGComment >> content: aString [

	self backend forBehavior setClassCommentContentFor: self to: aString.

	self announcer behaviorCommentModified: self parent
]

{ #category : 'default model values' }
RGComment >> defaultAuthor [

	^ ''
]

{ #category : 'default model values' }
RGComment >> defaultContent [

	^ ''
]

{ #category : 'managing container' }
RGComment >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : 'default model values' }
RGComment >> defaultTime [

	^ DateAndTime new
]

{ #category : 'accessing' }
RGComment >> fullName [

	^ (self parent name, ' comment') asSymbol
]

{ #category : 'testing' }
RGComment >> ifEmpty: aBlock [

	^ content ifEmpty: aBlock
]

{ #category : 'initialization' }
RGComment >> initialize [

	super initialize.

	content := self unresolvedValue: self defaultContent.
	author := self unresolvedValue: self defaultAuthor.
	time := self unresolvedValue: self defaultTime
]

{ #category : 'initialization' }
RGComment >> initializeUnresolved [

	super initializeUnresolved.

	content := self unresolvedValue: self defaultContent.
	author := self unresolvedValue: self defaultAuthor.
	time := self unresolvedValue: self defaultTime
]

{ #category : 'testing' }
RGComment >> isComment [

	^ true
]

{ #category : 'testing' }
RGComment >> isEmptyOrNil [

	^ self content isEmptyOrNil
]

{ #category : 'resolving' }
RGComment >> makeResolved [

	super makeResolved.

	content := self content markAsRingResolved.
	author := self author markAsRingResolved.
	time := self time markAsRingResolved
]

{ #category : 'printing' }
RGComment >> printOn: aStream [

	self parent name ifNotNil: [
		aStream nextPutAll: self parent name;
				  nextPutAll: ' ' ].
	aStream nextPutAll: self name asString
]

{ #category : 'private - backend interface' }
RGComment >> pvtAuthor [

	^ author value
]

{ #category : 'private - backend interface' }
RGComment >> pvtAuthor: aString [

	^ author := aString
]

{ #category : 'private - backend interface' }
RGComment >> pvtContent [

	^ content value
]

{ #category : 'private - backend interface' }
RGComment >> pvtContent: aString [

	^ content := aString
]

{ #category : 'private - backend interface' }
RGComment >> pvtResolvableProperties [

	^ super pvtResolvableProperties, {
		#content -> content.
		#author -> author.
		#time -> time.
	}
]

{ #category : 'private - backend interface' }
RGComment >> pvtTime [

	^ time value
]

{ #category : 'private - backend interface' }
RGComment >> pvtTime: aDateAndTime [

	^ time := aDateAndTime
]

{ #category : 'backward compatibility' }
RGComment >> sourceCode [

	^ self content
]

{ #category : 'accessing - backend' }
RGComment >> time [

	^ self backend forBehavior classCommentTimeFor: self
]

{ #category : 'accessing - backend' }
RGComment >> time: aDateAndTime [

	self backend forBehavior setClassCommentTimeFor: self to: aDateAndTime.

	self announcer behaviorCommentModified: self parent
]
