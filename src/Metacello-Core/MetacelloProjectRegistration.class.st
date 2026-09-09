Class {
	#name : 'MetacelloProjectRegistration',
	#superclass : 'Object',
	#instVars : [
		'loadedInImage',
		'locked',
		'projectSpec'
	],
	#category : 'Metacello-Core-Registry',
	#package : 'Metacello-Core',
	#tag : 'Registry'
}

{ #category : 'instance creation' }
MetacelloProjectRegistration class >> projectSpec: aProjectSpec [

	^ self new
		  projectSpec: aProjectSpec;
		  yourself
]

{ #category : 'querying' }
MetacelloProjectRegistration class >> projectSpecForClassNamed: aClassName ifAbsent: absentBlock [

	^ MetacelloProjectRegistry registry projectSpecForClassNamed: aClassName ifAbsent: absentBlock
]

{ #category : 'registration' }
MetacelloProjectRegistration class >> registrationForClassNamed: aClassName ifAbsent: absentBlock [
	"This method should not be used but is kept for SmalltalkCI compatibility. Use the same one on MetacelloProjectRegistry."

	^ MetacelloProjectRegistry registry registrationNamed: aClassName ifPresent: [ :registration | registration ] ifAbsent: absentBlock
]

{ #category : 'registration' }
MetacelloProjectRegistration class >> registry [
	"To deprecate once Iceberg is updated"

	^ MetacelloProjectRegistry registry
]

{ #category : 'comparing' }
MetacelloProjectRegistration >> = aRegistration [

	aRegistration class == self class ifFalse: [ ^ false ].
	^ projectSpec className = aRegistration className
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> baseName [
	^ self projectSpec baseName
]

{ #category : 'testing' }
MetacelloProjectRegistration >> canUpgradeTo: aProjectRegistration [

	^ projectSpec canUpgradeTo: aProjectRegistration projectSpec
]

{ #category : 'introspection' }
MetacelloProjectRegistration >> className [

	^ self projectSpec className
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> currentVersionString [

	^ self versionString
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> currentlyLoadedClassesInProject [
	"Used by SmalltalkCI."

	^ self projectSpec currentlyLoadedClassesInVersion asSet
]

{ #category : 'testing' }
MetacelloProjectRegistration >> hasLoadConflicts: aProjectRegistration [

	^ aProjectRegistration projectSpec hasLoadConflicts: self projectSpec
]

{ #category : 'comparing' }
MetacelloProjectRegistration >> hash [

	^ String stringHash: projectSpec metacelloRegistrationHash initialHash: 0
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> loadedInImage [

	^ loadedInImage ifNil: [ loadedInImage := false ]
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> loadedInImage: anObject [

	loadedInImage := anObject
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> locked [
    locked ifNil: [ locked := false ].
    ^ locked
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> locked: anObject [

	locked := anObject
]

{ #category : 'printing' }
MetacelloProjectRegistration >> printOn: aStream [

	| descriptions |
	aStream
		nextPutAll: self projectSpec className;
		space;
		nextPutAll: '[baseline]'.
	(descriptions := self repositoryDescriptions) isEmpty ifTrue: [ ^ self ].
	aStream nextPutAll: ' from '.
	descriptions size = 1
		ifTrue: [ aStream nextPutAll: descriptions first ]
		ifFalse: [
				aStream nextPut: ${.
				descriptions do: [ :description | aStream nextPutAll: description ].
				aStream nextPut: $} ]
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> projectName [

	^ self projectSpec name
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> projectSpec [

	^ projectSpec
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> projectSpec: anObject [

	projectSpec := anObject
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> projectVersion [

	^ self projectSpec version
]

{ #category : 'registration' }
MetacelloProjectRegistration >> registerProject [
	"unconditionally register <newRegistration> ... use with care"

	MetacelloProjectRegistry registry registerProjectRegistration: self
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> repositoryDescriptions [

	^ self projectSpec repositoryDescriptions
]

{ #category : 'registration' }
MetacelloProjectRegistration >> unregisterProject [

	MetacelloProjectRegistry registry unregisterProjectRegistration: self
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> versionInfo [
	"Deprecated in Pharo 14"
	^ self
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> versionString [

	^ projectSpec repositoryVersionString
]
