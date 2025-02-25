Class {
	#name : 'PBLogger',
	#superclass : 'Object',
	#instVars : [
		'queue',
		'logging',
		'depth'
	],
	#category : 'PharoBootstrap',
	#package : 'PharoBootstrap'
}

{ #category : 'logging' }
PBLogger >> basicLog: anItem [
	"I print the date and time in human readable format and tabulate the logs depending on the depth."

	| stream |
	stream := Stdio stdout.
	Time now print24: true on: stream.

	depth timesRepeat: [ stream nextPut: Character tab ].

	stream
		nextPutAll: anItem value asString;
		lf;
		flush
]

{ #category : 'logging' }
PBLogger >> execute: aBlock logged: aString [

	| result errorMessage |
	depth := depth + 1.
	self log: 'Begin: ' , aString.
	depth := depth + 1.
	[
	[ result := aBlock cull: aString ]
		on: Error
		do: [ :exception | "If there is an error, we keep the message and pass it. The message will be used in the #ifCurtailed: to ensure we keep the right indentation and print a better result to the user."
			errorMessage := exception printString.
			exception pass ] ] ifCurtailed: [ "If there is an unexpected termination of the execution, print that there was an error."
		depth := depth - 1.
		self log: 'End with error: ' , aString , '.' , (errorMessage
				 ifNil: [ '' ]
				 ifNotNil: [ 'Error message: "' , errorMessage , '"' ]).
		depth := depth - 1 ].

	depth := depth - 1.
	self log: 'End: ' , aString.
	depth := depth - 1.
	^ result
]

{ #category : 'logging' }
PBLogger >> initialize [

	super initialize.
	queue := AtomicSharedQueue new.
	logging := false.
	depth := 0
]

{ #category : 'logging' }
PBLogger >> log: aLogStatement [

	queue nextPut: aLogStatement.
	logging ifFalse: [
		logging := true.
		[ [ self basicLog: queue next ] repeat ] fork ].
]
