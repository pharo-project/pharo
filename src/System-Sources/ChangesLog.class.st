"
I represent the changes file and give access to him.  If you want to log a change, look at my #logChange: method.

I have a default instance that is hearing SystemAnnouncements and logging the change logs in there.
But you can have your own instance and log whatever you want.
"
Class {
	#name : #ChangesLog,
	#superclass : #Object,
	#instVars : [
		'startupStamp'
	],
	#classVars : [
		'DefaultInstance'
	],
	#category : #'System-Sources'
}

{ #category : #accessing }
ChangesLog class >> default [
	^DefaultInstance ifNil: [ 
		DefaultInstance := self new.
		self registerInterestToSystemAnnouncement.
		DefaultInstance.].
]

{ #category : #accessing }
ChangesLog class >> registerInterestToSystemAnnouncement [
	<systemEventRegistration>
	
	DefaultInstance ifNotNil: [:defaultInstance |
		SystemAnnouncer uniqueInstance unsubscribe: defaultInstance.
		defaultInstance registerToAnnouncements. ].

]

{ #category : #accessing }
ChangesLog class >> reset [
	SystemAnnouncer uniqueInstance unsubscribe: DefaultInstance.
	DefaultInstance := nil.
]

{ #category : #private }
ChangesLog >> assureStartupStampLogged [
	"If there is a startup stamp not yet actually logged to disk, do it now."

	startupStamp ifNil: [ ^ self ].

	SourceFiles changesWriteStreamDo: [ :changesFile |
		changesFile
			cr; cr;
			nextChunkPut: startupStamp asString;
			cr.
		startupStamp := nil ].

	SourceFiles forceChangesToDisk
]

{ #category : #logging }
ChangesLog >> logChange: aStringOrText [
	"Write the argument, aString, onto the changes file."

	| aString |
	SourceFiles changesFileStream closed ifTrue: [ ^ self ].
	
	self assureStartupStampLogged.

	aString := aStringOrText asString.
	(aString findFirst: [ :char | char isSeparator not ]) = 0
		ifTrue: [ ^ self ].	"null doits confuse replay"

	SourceFiles changesWriteStreamDo: [ :changesFile |
		changesFile
			cr; cr.
			
		(ChunkWriteStream on: changesFile)
			nextPut: aString ].

	SourceFiles forceChangesToDisk
]

{ #category : #'event-listening' }
ChangesLog >> logClassRemoved: annoucement [
	annoucement classRemoved acceptsLoggingOfCompilation ifTrue: [
		self logChange: 'Smalltalk globals removeClassNamed: #', annoucement classRemoved name	
	].

]

{ #category : #'event-listening' }
ChangesLog >> logClassRenamed: annoucement [
	annoucement classRenamed acceptsLoggingOfCompilation ifTrue: [
		self logChange: '(Smalltalk globals at: #', annoucement oldName, ') rename: #', annoucement newName.	
	].

]

{ #category : #'event-listening' }
ChangesLog >> logExpressionEvaluated: announcement [
	self logChange: announcement expressionEvaluated.
]

{ #category : #'event-listening' }
ChangesLog >> logMethodRemoved: announcement [
	announcement methodClass acceptsLoggingOfCompilation ifTrue: [
		self logChange: 
			announcement methodClass name, ' removeSelector: #', announcement selector
	]
	
]

{ #category : #logging }
ChangesLog >> logSnapshot: save andQuit: quit [
	"Log into the changes file the fact that the session will be either snapshoted, quit, or both."

	| message |	
	SourceFiles changesFileStream closed ifTrue: [ ^ self ].

	message := String
		streamContents: [ :s | 
			s
				nextPutAll: '----';
				nextPutAll:
						(save
								ifTrue: [ 
									quit
										ifTrue: [ 'QUIT' ]
										ifFalse: [ 'SNAPSHOT' ] ]
								ifFalse: [ 
									quit
										ifTrue: [ 'QUIT/NOSAVE' ]
										ifFalse: [ 'NOP' ] ]);
				nextPutAll: '----';
				print: DateAndTime now;
				space;
				nextPutAll: (Smalltalk imageFile basename);
				nextPutAll: ' priorSource: ';
				print: Smalltalk lastQuitLogPosition ].

	self assureStartupStampLogged.

	save ifTrue: [ 
		Smalltalk lastQuitLogPosition:
			SourceFiles changesFileStream size ].

	self logChange: message
]

{ #category : #accessing }
ChangesLog >> recordStartupStamp [
	
	startupStamp := '----STARTUP----', DateAndTime now printString, ' as ', Smalltalk imagePath.

]

{ #category : #'event-listening' }
ChangesLog >> registerToAnnouncements [
	SystemAnnouncer uniqueInstance weak
		when: ClassRemoved 
			send: #logClassRemoved:
			to: self;
		when: ClassRenamed 
			send: #logClassRenamed:
			to: self;
		when: MethodRemoved
			send: #logMethodRemoved:
			to: self;
		when: ExpressionEvaluated 
			send: #logExpressionEvaluated:
			to: self.
]
