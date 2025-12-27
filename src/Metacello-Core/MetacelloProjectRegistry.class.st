Class {
	#name : 'MetacelloProjectRegistry',
	#superclass : 'Object',
	#instVars : [
		'projectSpecsRegistry'
	],
	#category : 'Metacello-Core-Registry',
	#package : 'Metacello-Core',
	#tag : 'Registry'
}

{ #category : 'accessing' }
MetacelloProjectRegistry >> isEmpty [

	^ self baselineProjectSpecs isEmpty
]

{ #category : 'copying' }
MetacelloProjectRegistry >> postCopy [

	super postCopy.
	projectSpecsRegistry := self projectSpecsRegistry copy
]

{ #category : 'querying' }
MetacelloProjectRegistry >> projectSpecForClassNamed: aClassName ifAbsent: absentBlock [

	^ (self projectSpecsRegistry at: aClassName ifAbsent: [ ^ absentBlock value ]) projectSpec
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> projectSpecs [

	^ self projectSpecsRegistry values
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> projectSpecsRegistry [

	^ projectSpecsRegistry ifNil: [ projectSpecsRegistry := Dictionary new ]
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registerProjectRegistration: aMetacelloProjectRegistration [
	"unconditionally register <newRegistration> ... use with care"

	| spec |
	spec := aMetacelloProjectRegistration projectSpec.
	self projectSpecsRegistry at: spec className ifPresent: [ :existing |
			(existing projectSpec registrationsCompareEqual: spec) ifFalse: [
				MetacelloNotification signal: 'REGISTRATION OF INCOMPATABLE PROJECTS: ' , existing printString , ' REPLACED BY ' , aMetacelloProjectRegistration printString ] ].
	aMetacelloProjectRegistration versionInfo setVersionString: spec repositoryVersionString.
	spec immutable.
	self projectSpecsRegistry at: spec className put: aMetacelloProjectRegistration.
	aMetacelloProjectRegistration immutable
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registrationFor: aMetacelloProjectRegistration ifPresent: presentBlock ifAbsent: absentBlock [

	self projectSpecsRegistry at: aMetacelloProjectRegistration projectSpec className ifPresent: [ :existing | ^ presentBlock value: existing ].
	self projectSpecsRegistry at: 'BaselineOf' , aMetacelloProjectRegistration baseName ifPresent: [ :existing | ^ presentBlock value: existing ].
	^ absentBlock value
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registrationForClassNamed: aClassName ifAbsent: absentBlock [

	^ self registrationForExactClassNamed: aClassName ifAbsent: [
			  self projectSpecsRegistry at: (MetacelloScriptExecutor baselineNameFrom: aClassName) ifPresent: [ :registration | ^ registration ].
			  absentBlock value ]
]

{ #category : 'querying' }
MetacelloProjectRegistry >> registrationForExactClassNamed: aClassName ifAbsent: absentBlock [

	self projectSpecsRegistry at: aClassName ifPresent: [ :registration | ^ registration ].
	^ absentBlock value
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> registrations [

	^ self projectSpecsRegistry values
]

{ #category : 'registration' }
MetacelloProjectRegistry >> unregisterProjectRegistration: aMetacelloProjectRegistration [

	self projectSpecsRegistry removeKey: aMetacelloProjectRegistration projectSpec className ifAbsent: [ self error: 'unexpectedly missing project registration' ]
]
