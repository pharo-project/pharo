Class {
	#name : #PBBootstrapLogger,
	#superclass : #Object,
	#instVars : [
		'queue',
		'logging'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #'logging-Deprecated' }
PBBootstrapLogger >> basicLog: anItem [

	FileStream stdout nextPutAll: anItem value asString ;cr.
]

{ #category : #'logging-Deprecated' }
PBBootstrapLogger >> initialize [

	super initialize.
	queue := AtomicSharedQueue new.
	logging := false.
]

{ #category : #'logging-Deprecated' }
PBBootstrapLogger >> log: aLogStatement [

	queue nextPut: aLogStatement.
	logging ifFalse: [
		logging := true.
		[ [ self basicLog: queue next ] repeat ] fork ].
]
