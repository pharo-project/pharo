Class {
	#name : 'MetacelloProjectRegistry',
	#superclass : 'Object',
	#instVars : [
		'registrations'
	],
	#classVars : [
		'Registry'
	],
	#category : 'Metacello-Core-Registry',
	#package : 'Metacello-Core',
	#tag : 'Registry'
}

{ #category : 'baselines' }
MetacelloProjectRegistry class >> baselineClasses [
	"Return a set of the Metacello baseline classes that have been loaded into the image."

	^ BaselineOf allSubclasses
]

{ #category : 'copying' }
MetacelloProjectRegistry class >> copyRegistryDuring: aBlock commitIfSuccess: aBoolean [
	"install copy of registry for duration of <aBlock> execution."

	"registrations will be copied on write during <aBlock> execution."

	"if <aBlock> does not return control to this context, revert to the original
	version of the registry. Otherwise leave the new copy installed."

	| oldRegistry newRegistry |
	oldRegistry := self registry.
	newRegistry := self registry copy.
	self registry: newRegistry.
	aBlock ensure: [ "install old version of registry"
		newRegistry := self registry. "see https://github.com/dalehenrich/metacello-work/issues/210"
		self registry: oldRegistry ].

	aBoolean ifTrue: [ self registry: newRegistry ]
]

{ #category : 'accessing' }
MetacelloProjectRegistry class >> projects [

	^ self registry projects
]

{ #category : 'registration' }
MetacelloProjectRegistry class >> registrationNamed: aString ifPresent: presentBlock [

	^ self registry registrationNamed: aString ifPresent: presentBlock ifAbsent: [ ]
]

{ #category : 'registration' }
MetacelloProjectRegistry class >> registrationNamed: aString ifPresent: presentBlock ifAbsent: absentBlock [

	^ self registry registrationNamed: aString ifPresent: presentBlock ifAbsent: absentBlock
]

{ #category : 'registry' }
MetacelloProjectRegistry class >> registry [

	^ Registry ifNil: [ Registry := self new ]
]

{ #category : 'accessing' }
MetacelloProjectRegistry class >> registry: aMetacelloProjectRegistry [

	Registry := aMetacelloProjectRegistry
]

{ #category : 'initialization' }
MetacelloProjectRegistry class >> resetRegistry [

	<script>
	Registry := nil
]

{ #category : 'accessing' }
MetacelloProjectRegistry class >> useRegistry: aRegistry during: aBlock [

	| oldRegistry |
	oldRegistry := Registry.
	Registry := MetacelloProjectRegistry new.
	[ aBlock value ] ensure: [ Registry := oldRegistry ]
]

{ #category : 'baselines' }
MetacelloProjectRegistry >> baselineNameFrom: baseName [
	"Return the fully-qualified configuration class name."

	^ (baseName indexOfSubCollection: 'BaselineOf') > 0
		  ifTrue: [ baseName ]
		  ifFalse: [ 'BaselineOf' , baseName ]
]

{ #category : 'initialization' }
MetacelloProjectRegistry >> initialize [

	super initialize.
	registrations := OrderedCollection new
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> isEmpty [

	^ self registrations isEmpty
]

{ #category : 'copying' }
MetacelloProjectRegistry >> postCopy [

	super postCopy.
	registrations := registrations copy
]

{ #category : 'querying' }
MetacelloProjectRegistry >> projectSpecForClassNamed: aClassName ifAbsent: absentBlock [

	^ self registrationNamed: aClassName ifPresent: [ :existing | existing projectSpec ] ifAbsent: absentBlock
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> projects [

	^ self registrations collect: [ :registration | registration projectSpec ]
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registerProjectRegistration: aMetacelloProjectRegistration [
	"unconditionally register <newRegistration> ... use with care"

	self
		registrationNamed: aMetacelloProjectRegistration className
		ifPresent: [ :existing | MetacelloNotification signal: 'REGISTRATION OF INCOMPATABLE PROJECTS: ' , existing printString , ' REPLACED BY ' , aMetacelloProjectRegistration printString ]
		ifAbsent: [ self registrations add: aMetacelloProjectRegistration ]
]

{ #category : 'registration' }
MetacelloProjectRegistry >> registrationNamed: aClassName ifPresent: aBlock ifAbsent: absentBlock [

	| className |
	className := self baselineNameFrom: aClassName.

	^ self registrations
		  detect: [ :registration | registration className = className ]
		  ifFound: aBlock
		  ifNone: absentBlock
]

{ #category : 'accessing' }
MetacelloProjectRegistry >> registrations [

	^ registrations
]

{ #category : 'registration' }
MetacelloProjectRegistry >> unregisterProjectRegistration: aMetacelloProjectRegistration [

	self
		registrationNamed: aMetacelloProjectRegistration className
		ifPresent: [ :existing | self registrations remove: existing ]
		ifAbsent: [ self error: 'unexpectedly missing project registration' ]
]
