Class {
	#name : 'MetacelloProjectRegistration',
	#superclass : 'Object',
	#instVars : [
		'loadedInImage',
		'locked',
		'mutable',
		'versionInfo',
		'projectSpec'
	],
	#classVars : [
		'Registry'
	],
	#category : 'Metacello-Core-Registry',
	#package : 'Metacello-Core',
	#tag : 'Registry'
}

{ #category : 'accessing' }
MetacelloProjectRegistration class >> baselineClasses [
    "Return a set of the Metacello baseline classes that have been loaded into the image."

    "self baselineClasses"

    ^ BaselineOf allSubclasses
]

{ #category : 'mutability' }
MetacelloProjectRegistration class >> copyRegistryDuring: aBlock commitIfSuccess: aBoolean [
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

{ #category : 'instance creation' }
MetacelloProjectRegistration class >> projectSpec: aProjectSpec [

	^ self new
		  projectSpec: aProjectSpec;
		  yourself
]

{ #category : 'querying' }
MetacelloProjectRegistration class >> projectSpecForClassNamed: aClassName ifAbsent: absentBlock [
    ^ self registry projectSpecForClassNamed: aClassName ifAbsent: absentBlock
]

{ #category : 'accessing' }
MetacelloProjectRegistration class >> projectSpecs [

	^ self registry baselineProjectSpecs
]

{ #category : 'registration' }
MetacelloProjectRegistration class >> registrationForClassNamed: aClassName ifAbsent: absentBlock [

	^ self registry registrationForClassNamed: aClassName ifAbsent: absentBlock
]

{ #category : 'querying' }
MetacelloProjectRegistration class >> registrationForProjectSpec: aProjectSpec ifAbsent: absentBlock ifPresent: presentBlock [
    | newRegistration |
    newRegistration := aProjectSpec asProjectRegistration.
    self registry
        registrationFor: newRegistration
        ifPresent: [ :existing | ^ presentBlock value: existing value: newRegistration ]
        ifAbsent: [ ^ absentBlock value: newRegistration ]
]

{ #category : 'accessing' }
MetacelloProjectRegistration class >> registry [

	^ Registry ifNil: [ Registry := MetacelloProjectRegistry new ]
]

{ #category : 'accessing' }
MetacelloProjectRegistration class >> registry: aMetacelloProjectRegistry [
    Registry := aMetacelloProjectRegistry
]

{ #category : 'accessing' }
MetacelloProjectRegistration class >> resetRegistry [

	<script>
	Registry := nil
]

{ #category : 'accessing' }
MetacelloProjectRegistration class >> useRegistry: aRegistry during: aBlock [

	| oldRegistry |
	oldRegistry := Registry.
	Registry := MetacelloProjectRegistry new.
	[ aBlock value ] ensure: [ Registry := oldRegistry ]
]

{ #category : 'comparing' }
MetacelloProjectRegistration >> = aRegistration [

	aRegistration class == self class ifFalse: [ ^ false ].
	^ projectSpec registrationsCompareEqual: aRegistration projectSpec
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> baseName [
	^ self projectSpec baseName
]

{ #category : 'testing' }
MetacelloProjectRegistration >> canDowngradeTo: aProjectRegistration [
	"true if there are no load conflicts
        OR
       if the load conflicts involved two cofigurations ONLY and a downgrade is allowed"

	(self hasLoadConflicts: aProjectRegistration) ifFalse: [ ^ true ].

	^ false
]

{ #category : 'testing' }
MetacelloProjectRegistration >> canUpgradeTo: aProjectRegistration [
	"true if there are no load conflicts
        OR
       if the load conflicts involved two cofigurations ONLY and an upgrade is allowed"

	(self hasLoadConflicts: aProjectRegistration) ifFalse: [ ^ true ].

	projectSpec ifNotNil: [ ^ projectSpec canUpgradeTo: aProjectRegistration projectSpec ].
	
	^ false
]

{ #category : 'mutability' }
MetacelloProjectRegistration >> copyOnWrite: aBlock [
	"assume that only registered projects are immutable ... otherwise you'll get an error"

	| copy |
	self class registry registrationFor: self ifPresent: [ :existing |  ] ifAbsent: [
		aBlock value: self.
		^ self ].
	self unregisterProject.
	copy := self copy.
	aBlock value: copy.
	copy registerProject.
	^ copy
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> currentVersionString [

	^ projectSpec repositoryVersionString
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> currentlyLoadedClassesInProject [
	"Used by SmalltalkCI."

	^ self projectSpec currentlyLoadedClassesInVersion asSet
]

{ #category : 'testing' }
MetacelloProjectRegistration >> hasLoadConflicts: aProjectRegistration [

	aProjectRegistration validate.
	^ self projectSpec hasConflictWithProjectSpec: aProjectRegistration projectSpec
]

{ #category : 'comparing' }
MetacelloProjectRegistration >> hash [

	^ String stringHash: projectSpec metacelloRegistrationHash initialHash: 0
]

{ #category : 'mutability' }
MetacelloProjectRegistration >> immutable [
    mutable := false
]

{ #category : 'mutability' }
MetacelloProjectRegistration >> isMutable [

	^ mutable ifNil: [ true ]
]
{ #category : 'accessing' }
MetacelloProjectRegistration >> loadedInImage [

	^ loadedInImage ifNil: [ loadedInImage := false ]
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> loadedInImage: anObject [
    self shouldBeMutable.
    loadedInImage := anObject
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> locked [
    locked ifNil: [ locked := false ].
    ^ locked
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> locked: anObject [
    self shouldBeMutable.
    locked := anObject
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> merge: aProjectRegistration [
	" ... merge is done when a spec has been loaded into the image"

	self shouldBeMutable.
	aProjectRegistration validate.

	projectSpec := aProjectRegistration projectSpec.
	self versionInfo versionString: projectSpec repositoryVersionString
]

{ #category : 'mutability' }
MetacelloProjectRegistration >> mutable [
    mutable := true
]

{ #category : 'copying' }
MetacelloProjectRegistration >> postCopy [
    super postCopy.
    mutable := nil
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

	self shouldBeMutable.
	projectSpec := anObject
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> projectVersion [

	^ self versionInfo projectVersion ifNil: [
			  | version |
			  version := self projectSpec versionOrNil.
			  self versionInfo projectVersion: version.
			  version ]
]

{ #category : 'registration' }
MetacelloProjectRegistration >> registerProject [
    "unconditionally register <newRegistration> ... use with care"

    self class registry registerProjectRegistration: self
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> repositoryDescriptions [

	^ self projectSpec repositoryDescriptions
]

{ #category : 'mutability' }
MetacelloProjectRegistration >> shouldBeMutable [

	self isMutable ifTrue: [ ^ self ].
	self error: 'Not allowed to modify an immutable object'
]

{ #category : 'registration' }
MetacelloProjectRegistration >> unregisterProject [
    self class registry unregisterProjectRegistration: self
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> validate [

	projectSpec ifNil: [ self error: 'Invalid project registration' ]
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> version [

	^ MetacelloProject singletonVersionName
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> versionInfo [

	^ versionInfo ifNil: [ versionInfo := MetacelloProjectRegistrationVersionInfo new ]
]

{ #category : 'accessing' }
MetacelloProjectRegistration >> versionInfo: aMetacelloProjectRegistrationVersionInfo [
  versionInfo := aMetacelloProjectRegistrationVersionInfo
]
