"
I am an error handler for non interactive image management
"
Class {
	#name : 'NonInteractiveErrorHandler',
	#superclass : 'ErrorHandler',
	#instVars : [
		'doNotQuitOnRestart'
	],
	#classVars : [
		'SnapshotErrorImage'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'testing' }
NonInteractiveErrorHandler class >> isValidForCurrentSystemConfiguration [

	^ Smalltalk isInteractive not
]

{ #category : 'accessing' }
NonInteractiveErrorHandler class >> snapshotErrorImage [
	^ SnapshotErrorImage == true
]

{ #category : 'accessing' }
NonInteractiveErrorHandler class >> snapshotErrorImage: aBoolean [
	SnapshotErrorImage := aBoolean
]

{ #category : 'events' }
NonInteractiveErrorHandler >> exitFailure [
	[
		self class snapshotErrorImage ifTrue: [
			doNotQuitOnRestart := true.
			"make a new image version snapshot before leaving"
			Smalltalk saveAsNewVersion  ].
	] ensure: [
		doNotQuitOnRestart ifFalse: [ Smalltalk exitFailure ].
		doNotQuitOnRestart := false.
	]
]

{ #category : 'error handling' }
NonInteractiveErrorHandler >> handleError: anError log: shouldLog [

	self quitFrom: anError signalerContext withMessage: anError description
]

{ #category : 'error handling' }
NonInteractiveErrorHandler >> handleNotification: aNotification [

	Smalltalk logStdOutDuring: [:logger |
		logger nextPutAll: aNotification class name;
				 nextPutAll: ': ';
				 nextPutAll: aNotification messageText;
				 cr ].

	aNotification resume
]

{ #category : 'error handling' }
NonInteractiveErrorHandler >> handleWarning: aWarning [
	| logBlock |
	"Pass all warnings, but log them"

	logBlock := [:logger |
		logger
			cr;
			nextPutAll: '*** Warning: ';
			nextPutAll: aWarning description;
			cr ].

	Smalltalk logDuring: logBlock.
	self logYellowDuring: logBlock.

	aWarning resume
]

{ #category : 'initialization' }
NonInteractiveErrorHandler >> initialize [
	doNotQuitOnRestart := false
]

{ #category : 'error handling' }
NonInteractiveErrorHandler >> logColored: anAnsiiColorCode during: aBlock [

	Smalltalk logStdErrorDuring: [ :stderr |
			stderr
				nextPut: Character escape;
				nextPut: $[;
				nextPutAll: anAnsiiColorCode;
				nextPut: $m.

			aBlock value: stderr.

			stderr
				nextPut: Character escape;
				nextPutAll: '[0m' ]
]

{ #category : 'error handling' }
NonInteractiveErrorHandler >> logYellowDuring: aBlock [

	^ self logColored: '33' during: aBlock
]

{ #category : 'events' }
NonInteractiveErrorHandler >> quitFrom: aContext withMessage: aString [

	" log error and quit "
	[ Smalltalk
		logError: aString
		inContext: aContext.

	" Print stacks of all current processes "

	Smalltalk logDuring: [:logger |
		logger nextPutAll: 'Processes and their stacks: ';cr.

		Process allInstances do: [:each | | ctx |
			logger nextPutAll: 'Process: '; print: each; cr; nextPutAll: '  stack:'; cr; cr.

			ctx := each isActive ifTrue: [ thisContext sender ] ifFalse: [ each suspendedContext ].
			ctx ifNotNil: [
				(ctx stackOfSize: 20) do: [:s | logger print: s; cr ]].
			logger nextPutAll: '------------------------------'; cr; cr.
		]].
	] ensure: [ self exitFailure ]
]
