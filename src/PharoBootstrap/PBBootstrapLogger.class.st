Class {
	#name : 'PBBootstrapLogger',
	#superclass : 'Object',
	#instVars : [
		'queue',
		'logging'
	],
	#category : 'PharoBootstrap',
	#package : 'PharoBootstrap'
}

{ #category : 'logging-Deprecated' }
PBBootstrapLogger >> basicLog: anItem [

	Stdio stdout
		nextPutAll: anItem value asString;
		lf;
		flush
]

{ #category : 'logging-Deprecated' }
PBBootstrapLogger >> initialize [

	super initialize.
	queue := AtomicSharedQueue new.
	logging := false.
]

{ #category : 'logging-Deprecated' }
PBBootstrapLogger >> log: aLogStatement [

	queue nextPut: aLogStatement.
	logging ifFalse: [
		logging := true.
		[ [ self basicLog: queue next ] repeat ] fork ].
]
