"
I am an abstract class to manage sources in Pharo.

I define the API a sources manager should have and hold the current sources manager.

I can have different subclasses with different ways to store the source code:
- A null sources manager
- A sources manager saving the code in files
- A source manager saving the code in a database...

For more detail, check my subclasses
"
Class {
	#name : 'SourcesManager',
	#superclass : 'Object',
	#classVars : [
		'Current'
	],
	#category : 'Kernel-Sources',
	#package : 'Kernel',
	#tag : 'Sources'
}

{ #category : 'query' }
SourcesManager class >> beCurrent [

	self isAbstract ifTrue: [ self error: 'Source manager cannot be abstract' ].

	self current class = self ifTrue: [ ^ self ].

	self current ifNotNil: [ self current shutDown ].
	self current: self new.
	self current startUp
]

{ #category : 'accessing' }
SourcesManager class >> current [

	^ Current
]

{ #category : 'accessing' }
SourcesManager class >> current: anObject [

	Current := anObject
]

{ #category : 'class initialization' }
SourcesManager class >> initialize [
	"Ensure we have a sources manager but do not force it"

	self current ifNil: [ NoSourcesManager beCurrent ]
]

{ #category : 'testing' }
SourcesManager class >> isAbstract [

	^ self = SourcesManager
]

{ #category : 'system startup' }
SourcesManager class >> shutDown: isImageClosing [

	isImageClosing ifTrue: [ self current shutDown ]
]

{ #category : 'system startup' }
SourcesManager class >> startUp: resuming [

	resuming ifTrue: [ self current startUp ]
]

{ #category : 'accessing' }
SourcesManager >> commentTimeStampAt: sourcePointer [

	^ self subclassResponsibility
]

{ #category : 'public - string writing' }
SourcesManager >> deferFlushDuring: aBlock [
	"If a specific implementation of SourcesManager is using a flush system, I can be overriden to defer the flushing during a block."

	aBlock value
]

{ #category : 'public - file system operations' }
SourcesManager >> ensureFlushing [
	"Ensure the sources manager flush its streams, if it has some"

	
]

{ #category : 'copying' }
SourcesManager >> imageMovedWithNewName: newNameWithoutSuffix [
	"I am called when the image is relocated in case the source manager needs to update itself following the change."

	self subclassResponsibility
]

{ #category : 'accessing' }
SourcesManager >> protocolAt: sourcePointer [

	^ self subclassResponsibility
]

{ #category : 'querying' }
SourcesManager >> reportString [

	^ self subclassResponsibility
]

{ #category : 'initialization' }
SourcesManager >> reset [

]

{ #category : 'system startup' }
SourcesManager >> shutDown [
	"Hook during image closing."
]

{ #category : 'accessing' }
SourcesManager >> sourceCodeAt: sourcePointer [

	^ self subclassResponsibility
]

{ #category : 'system startup' }
SourcesManager >> startUp [
	"Hook during image opening."
]

{ #category : 'accessing' }
SourcesManager >> timeStampAt: sourcePointer [

	^ self subclassResponsibility
]

{ #category : 'logging' }
SourcesManager >> writeClassCreation: aClass [

	self subclassResponsibility
]

{ #category : 'logging' }
SourcesManager >> writeClassRemoved: aClass [

	self subclassResponsibility
]

{ #category : 'logging' }
SourcesManager >> writeClassRenamed: aClass oldName: aString [

	self subclassResponsibility
]

{ #category : 'writing' }
SourcesManager >> writeComment: aString stamp: aStamp of: aClass [

	self subclassResponsibility
]

{ #category : 'logging' }
SourcesManager >> writeMethodRemoved: aMethod from: aBehavior [

	self subclassResponsibility
]

{ #category : 'writing' }
SourcesManager >> writeSource: source of: aMethod withFooter: footer protocol: protocol timestamp: aTimestamp priorSourcePointer: priorSourcePointer [

	self subclassResponsibility
]
