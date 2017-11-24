"
I am a helper for creating toy logs.
"
Class {
	#name : #EpTestLogBuilder,
	#superclass : #TestResource,
	#instVars : [
		'log',
		'directory'
	],
	#category : #'EpiceaTests-Resources'
}

{ #category : #releasing }
EpTestLogBuilder >> cleanUp [

	self directory ensureDeleteAll
]

{ #category : #accessing }
EpTestLogBuilder >> directory [

	^ directory ifNil: [ directory := self newDirectory ]
]

{ #category : #accessing }
EpTestLogBuilder >> log [

	^ log ifNil: [ log := self newLog ]
]

{ #category : #building }
EpTestLogBuilder >> logChildren: anInteger [ 
	self
		logChildren: anInteger 
		triggerReference: self logHeadReference
]

{ #category : #building }
EpTestLogBuilder >> logChildren: anInteger triggerReference: triggerReference [

	1 to: anInteger do: [ :index |
		self 
			logEvent: (EpExpressionEvaluation expression: index asString)
			triggerReference: triggerReference ].
]

{ #category : #building }
EpTestLogBuilder >> logEvent: anEvent [

	^ self 
		logEvent: anEvent 
		triggerReference: self log nullReference
]

{ #category : #building }
EpTestLogBuilder >> logEvent: anEvent triggerReference: triggerReference [
	self log 
		addEntryWith: anEvent 
		tags: [ :tags |
			tags at: EpLog authorKey put: 'TestAuthor'.
			tags at: EpLog timeKey put: DateAndTime now.
						
			triggerReference isNull ifFalse: [
				tags 
					at: EpLog triggererReferenceKey
					put: triggerReference ] ].
	self log store flush.
]

{ #category : #accessing }
EpTestLogBuilder >> logHeadReference [
	^ self log headReference
]

{ #category : #building }
EpTestLogBuilder >> logInitial [
	self
		logEvent: (EpExpressionEvaluation expression: '0')
		triggerReference: self log nullReference
]

{ #category : #initialization }
EpTestLogBuilder >> newDirectory [

	^ ('epicea-tests-', UUID new asString36) asFileReference
]

{ #category : #initialization }
EpTestLogBuilder >> newLog [

	^ EpLog newWithStore: self newStore
]

{ #category : #initialization }
EpTestLogBuilder >> newLogWithSessionStore [

	^ EpLog newWithStore: self newSessionStore
]

{ #category : #initialization }
EpTestLogBuilder >> newSessionStore [

	^ OmSessionStore newWithBaseLocator: self directory ensureCreateDirectory
]

{ #category : #initialization }
EpTestLogBuilder >> newStore [

	^ OmFileStore 
		named: 'tmp', UUID new asString36
		inDirectory: self directory ensureCreateDirectory
]

{ #category : #samples }
EpTestLogBuilder >> oneTriggerWithFourChildrenLog [

	| triggerReference |
	self logInitial.
	
	triggerReference := self logHeadReference.

	self
		logChildren: 2 
		triggerReference: triggerReference.

	self
		logEvent: (EpClassAddition class: Integer)
		triggerReference: triggerReference.

	self
		logChildren: 1
		triggerReference: triggerReference.

	^ self log
]

{ #category : #samples }
EpTestLogBuilder >> oneTriggerWithOneChildLog [

	| triggerReference |
	self logInitial.
	
	triggerReference := self logHeadReference.

	self
		logChildren: 2 
		triggerReference: triggerReference.

	self
		logEvent: (EpClassAddition class: Integer)
		triggerReference: triggerReference.
		
	self
		logEvent: (EpLogEntriesComment entryReferences: { self logHeadReference } oldComment: '' newComment: 'Passed Code Review')
		triggerReference: triggerReference.

	^ self log
]

{ #category : #initialization }
EpTestLogBuilder >> useLogWithSessionStore [

	log := self newLogWithSessionStore
]
