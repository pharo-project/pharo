Class {
	#name : 'MetacelloProjectRegistry',
	#superclass : 'Object',
	#instVars : [
		'baselineRegistry'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloProjectRegistry >> baselineProjectSpecs [
    "MetacelloProjectRegistration baselineProjectSpecs"

    | projectSpecs |
    projectSpecs := OrderedCollection new.
    self baselineRegistry
        keysAndValuesDo: [ :className :registration | projectSpecs add: (self projectSpecForClassNamed: className ifAbsent: [ self error: 'not expected' ]) ].
    ^ projectSpecs asArray
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> baselineRegistry [

	^ baselineRegistry ifNil: [ baselineRegistry := Dictionary new ]
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> isEmpty [

	^ self baselineProjectSpecs isEmpty
]

{ #category : 'copying' }
MetacelloProjectRegistry >> postCopy [

	super postCopy.
	baselineRegistry := self baselineRegistry copy
]

{ #category : 'querying' }
MetacelloProjectRegistry >> projectSpecForClassNamed: aClassName ifAbsent: absentBlock [

	^ (self baselineRegistry at: aClassName ifAbsent: [ ^ absentBlock value ]) baselineProjectSpec
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registerProjectRegistration: aMetacelloProjectRegistration [
	"unconditionally register <newRegistration> ... use with care"

	aMetacelloProjectRegistration baselineProjectSpec ifNotNil: [ :spec |
			self baselineRegistry at: spec className ifPresent: [ :existing |
					(existing baselineProjectSpec registrationsCompareEqual: spec) ifFalse: [
						MetacelloNotification signal:
							'REGISTRATION OF INCOMPATABLE PROJECTS: ' , existing printString , ' REPLACED BY ' , aMetacelloProjectRegistration printString ] ].
			(aMetacelloProjectRegistration respondsTo: #versionInfo) ifTrue: [ aMetacelloProjectRegistration versionInfo setVersionString: spec repositoryVersionString ].
			spec immutable.
			self baselineRegistry at: spec className put: aMetacelloProjectRegistration ].
	aMetacelloProjectRegistration immutable
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registrationFor: aMetacelloProjectRegistration ifPresent: presentBlock ifAbsent: absentBlock [

	| baseName |
	baseName := aMetacelloProjectRegistration baseName.

	aMetacelloProjectRegistration baselineProjectSpec ifNotNil: [ :spec |
		self baselineRegistry at: spec className ifPresent: [ :existing | ^ presentBlock value: existing ] ].
	self baselineRegistry at: 'BaselineOf' , baseName ifPresent: [ :existing | ^ presentBlock value: existing ].
	^ absentBlock value
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registrationForClassNamed: aClassName ifAbsent: absentBlock [

	^ self registrationForExactClassNamed: aClassName ifAbsent: [
			  | baseName |
			  baseName := MetacelloScriptEngine baseNameOf: aClassName.
			  self baselineRegistry at: 'BaselineOf' , baseName ifPresent: [ :registration | ^ registration ].
			  absentBlock value ]
]

{ #category : 'querying' }
MetacelloProjectRegistry >> registrationForExactClassNamed: aClassName ifAbsent: absentBlock [

	self baselineRegistry at: aClassName ifPresent: [ :registration | ^ registration ].
	^ absentBlock value
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> registrations [

	^ self baselineRegistry values
]

{ #category : 'registration' }
MetacelloProjectRegistry >> unregisterProjectRegistration: aMetacelloProjectRegistration [

	aMetacelloProjectRegistration baselineProjectSpec ifNotNil: [ :spec |
		self baselineRegistry removeKey: spec className ifAbsent: [ self error: 'unexpectedly missing project registration' ] ]
]
