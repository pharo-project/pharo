Class {
	#name : #IceExternalJob,
	#superclass : #Object,
	#instVars : [
		'title',
		'started',
		'min',
		'max',
		'current'
	],
	#category : #'Iceberg-Progress'
}

{ #category : #events }
IceExternalJob >> changed [

	current >= max ifTrue: [ ^ Job jobAnnouncer announce: (JobEnd on: self) ].
	Job jobAnnouncer announce: (JobChange on: self)
]

{ #category : #accessing }
IceExternalJob >> current: anInteger [ 
	current := anInteger 
]

{ #category : #events }
IceExternalJob >> end [
	Job jobAnnouncer announce: (JobEnd on: self)
]

{ #category : #events }
IceExternalJob >> ensureStarted [
	started ifFalse: [ self start ]
]

{ #category : #initialization }
IceExternalJob >> initialize [
	super initialize.
	started := false
]

{ #category : #accessing }
IceExternalJob >> max: anInteger [ 
	max := anInteger
]

{ #category : #accessing }
IceExternalJob >> min: anInteger [ 
	min := anInteger 
]

{ #category : #accessing }
IceExternalJob >> owner [
	^ nil
]

{ #category : #accessing }
IceExternalJob >> progress [
	^ min >= max ifTrue: [ 1 ] ifFalse: [ (current - min) / (max - min) ]
]

{ #category : #events }
IceExternalJob >> start [
	started := true.

	Job jobAnnouncer announce: (JobStart on: self)
]

{ #category : #accessing }
IceExternalJob >> title [
	^ title
]

{ #category : #accessing }
IceExternalJob >> title: aString [ 
	title := aString
]
