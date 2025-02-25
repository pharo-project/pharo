"
I represent the execution of an Snapshot of the image.
I hold all the parameters and information about the snapshot
I also hold information about the result of the excecution (if I am in a new session or not) and if there were errors.
"
Class {
	#name : 'SnapshotOperation',
	#superclass : 'Object',
	#instVars : [
		'isImageStarting',
		'error',
		'save',
		'quit',
		'sessionManager',
		'imageFormatOption'
	],
	#classVars : [
		'NextImageFormatOptionToUse'
	],
	#category : 'System-Support-Image',
	#package : 'System-Support',
	#tag : 'Image'
}

{ #category : 'instance creation' }
SnapshotOperation class >> new [

	self error: 'Use #save:anQuit:withSessionManager:'
]

{ #category : 'instance creation' }
SnapshotOperation class >> save: save andQuit: quit withSessionManager: sessionManager [

	| newOperation |

	newOperation := self basicNew.
	
	newOperation
		initialize;
		save: save;
		quit: quit;
		sessionManager: sessionManager;
		imageFormatOption: NextImageFormatOptionToUse.
	
	"I clear the next image format to use, nil value tells the VM to use previous set of there is not set to autodetect one"	
	NextImageFormatOptionToUse := nil.
	
	^ newOperation 
]

{ #category : 'configuring' }
SnapshotOperation class >> useComposedFormatNext [
	
	"VM Image format to use"
	NextImageFormatOptionToUse := 3	
]

{ #category : 'configuring' }
SnapshotOperation class >> useSpurFormatNext [
	
	"VM Image format to use"
	NextImageFormatOptionToUse := 4	
]

{ #category : 'private - operations' }
SnapshotOperation >> doSnapshot [

	isImageStarting := imageFormatOption 
		ifNil: [ self snapshotPrimitive ]
		ifNotNil: [ self snapshotPrimitiveWithFormatOption: imageFormatOption ] 

]

{ #category : 'error handling' }
SnapshotOperation >> error [ 

	^ error
]

{ #category : 'private - operations' }
SnapshotOperation >> executeStoringError: aBlock [

	^ aBlock onErrorDo: [ :e | e freeze. error := e ]
]

{ #category : 'private - primitives' }
SnapshotOperation >> handleSnapshotError: snapshotResult [

	| message |
	message := snapshotResult isSymbol 
		ifTrue: [ snapshotResult ] 
		ifFalse: [ 'Unknown Error while executing snapshot primitive' ].
		
	self error: message
]

{ #category : 'accessing' }
SnapshotOperation >> hasError [

	^ error isNotNil
]

{ #category : 'accessing' }
SnapshotOperation >> hasSavedSuccessfully [

	^ error isNil
]

{ #category : 'accessing' }
SnapshotOperation >> imageFormatOption: aValue [ 
	imageFormatOption := aValue
]

{ #category : 'initialization' }
SnapshotOperation >> initialize [

	super initialize.
	isImageStarting := false
]

{ #category : 'accessing' }
SnapshotOperation >> isImageStarting [
	
	^ isImageStarting 
]

{ #category : 'operations' }
SnapshotOperation >> performSnapshot [

	"If both false... nothing to do"
	(save or: [quit]) ifFalse: [ ^ self ].

	ChangesLog default logSnapshot: save andQuit: quit.

	"Image not usable from here until the session is restarted!"
	self executeStoringError: [sessionManager currentSession stop: quit].
	
	self hasError ifTrue: [ 
		"If there is an error in stop, I will try to restart the session"
		sessionManager currentSession start: false.
		^ self ].

	save
		ifTrue: [ self executeStoringError: [self doSnapshot]].
				
	(quit and: [ isImageStarting not ])
		ifTrue: [ Smalltalk quitPrimitive ].

	"create a new session object if we're booting"

	self executeStoringError: [ 
		isImageStarting ifTrue: [ sessionManager installNewSession ].
		sessionManager currentSession start: isImageStarting ].	
]

{ #category : 'accessing' }
SnapshotOperation >> quit: aValue [

	quit := aValue
]

{ #category : 'accessing' }
SnapshotOperation >> save: aValue [

	save := aValue
]

{ #category : 'accessing' }
SnapshotOperation >> sessionManager: aValue [

	sessionManager := aValue
]

{ #category : 'private - primitives' }
SnapshotOperation >> snapshotPrimitive [
	"Primitive. Write the current state of the object memory on a file in the
	same format as the Smalltalk-80 release. The file can later be resumed,
	returning you to this exact state. Return normally after writing the file.
	Essential. See Object documentation whatIsAPrimitive."

	"I will return
		true if the image is starting or
		false if the image is just resuming"

	<primitive: 97 error: #ec > 
	
	self handleSnapshotError: ec
]

{ #category : 'private - primitives' }
SnapshotOperation >> snapshotPrimitiveWithFormatOption: anArg [
	"Primitive. Write the current state of the object memory on a file in the
	same format as the Smalltalk-80 release. The file can later be resumed,
	returning you to this exact state. Return normally after writing the file.
	Essential. See Object documentation whatIsAPrimitive.
	
	My argument says the image format to use."

	"I will return
		true if the image is starting or
		false if the image is just resuming"

	<primitive: 97 error: #ec > 
	
	self handleSnapshotError: ec
]
