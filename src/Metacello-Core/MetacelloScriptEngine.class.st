"
MetacelloScriptEngine runs the execution of the script for one projectSpec
"
Class {
	#name : 'MetacelloScriptEngine',
	#superclass : 'Object',
	#instVars : [
		'options',
		'specBuilder',
		'projectSpec',
		'resolution',
		'directive',
		'fetched'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'private' }
MetacelloScriptEngine >> doLoad [
	"The directive, loaded, fetching first if needed"

	(MetacelloLoadTarget on: self) load: self ensureFetched.
	^ directive
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> doLoadProjectSpecFrom: aMetacelloPackageSpec [

	self loader
		loadAtomicPackageDirectives: { (MetacelloPackageLoadDirective spec: aMetacelloPackageSpec) }
		engine: self
]

{ #category : 'private' }
MetacelloScriptEngine >> during: aBlock keepingRegistry: aBoolean [
	"Run aBlock on a copy of the registry, with the script's handlers for upgrades, downgrades, conflicts and locks; keep the copy only when aBlock succeeds and aBoolean. Answer what aBlock answers"

	^ MetacelloProjectRegistry copyRegistryDuring: [ self handleNotificationsForAction: aBlock ] commitIfSuccess: aBoolean
]

{ #category : 'private' }
MetacelloScriptEngine >> ensureFetched [
	"The directive, its packages fetched once, recording first if needed"

	fetched == true ifTrue: [ ^ directive ].
	self ensureRecorded acceptVisitor: (MetacelloFetchTarget on: self).
	fetched := true.
	^ directive
]

{ #category : 'private' }
MetacelloScriptEngine >> ensureRecorded [
	"The directive loading the resolution, recorded once, resolving first if needed. A resolution with errors is never recorded: it signals MetacelloResolutionError"

	directive ifNotNil: [ ^ directive ].
	self ensureResolved hasErrors ifTrue: [ MetacelloResolutionError signalFor: resolution ].
	^ directive := self recordDirective
]

{ #category : 'private' }
MetacelloScriptEngine >> ensureResolved [
	"The resolution of the root, with the names its project spec loads, resolved once"

	resolution ifNotNil: [ ^ resolution ].
	self resolution: ((MetacelloResolveTarget on: self) resolve: self projectSpec).
	^ resolution
]

{ #category : 'actions api' }
MetacelloScriptEngine >> fetch [
	"Fetch what loading the default root needs, recording first if needed (see ensureFetched), and answer the directive"

	^ self during: [ self ensureFetched ] keepingRegistry: false
]

{ #category : 'actions api' }
MetacelloScriptEngine >> get [
	" load a fresh copy from repo"

	^ MetacelloProjectRegistry
		  copyRegistryDuring: [
				  self handleNotificationsForAction: [
						  | spec projectPackage |
						  spec := self projectSpec.
						  projectPackage := spec projectPackage.
						  projectPackage repositorySpecs do: [ :repoSpec | repoSpec createRepository flushForScriptGet ].
						  self doLoadProjectSpecFrom: projectPackage.
						  MetacelloProjectRegistry
							  registrationNamed: spec className
							  ifPresent: [ :existing | existing projectSpec repositories: spec repositories ]
							  ifAbsent: [ spec asProjectRegistration registerProject ].
						  (Smalltalk at: spec className asSymbol) project ] ]
		  commitIfSuccess: true
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleConflict: exception [
    ^ (self options at: #'onConflict' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleDowngrade: exception [
    ^ (self options at: #'onDowngrade' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleLock: exception [
  ^ (self options at: #'onLock' ifAbsent: [ ^ exception pass ])
    cull: exception
    cull: exception existingProjectRegistration
    cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleNotificationsForAction: actionBlock [

	^ [
		  [
			  actionBlock
				  on: MetacelloAllowProjectUpgrade , MetacelloAllowConflictingProjectUpgrade
				  do: [ :ex | "option handlers need to be outermost set of handlers ... last line of defense before users are involved"
					  ex handleResolutionFor: self ] ]
			  on: MetacelloAllowLockedProjectChange
			  do: [ :ex | "MetacelloAllowLockedProjectChange need to be outermost handler ... since it is signaled from second line of handlers"
				  ex handleResolutionFor: self ] ]
		  on: Warning
		  do: [ :ex | "Warning is absolute outermost handler" self handleWarning: ex ]
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleUpgrade: exception [
    ^ (self options at: #'onUpgrade' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleWarning: exception [
  ^ (self options at: #'onWarning' ifAbsent: [ ^ exception pass ])
    cull: exception
]

{ #category : 'options' }
MetacelloScriptEngine >> ignoreImage [
    ^ self options at: #'ignoreImage' ifAbsent: [ false ]
]

{ #category : 'initialization' }
MetacelloScriptEngine >> initialize [

	super initialize.
	specBuilder := MetacelloSpecBuilder new
]

{ #category : 'actions api' }
MetacelloScriptEngine >> list [

	^ self projectSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> load [
	"Load the default root, fetching first if needed (see doLoad), and keep the registry changes"

	^ self during: [ self doLoad ] keepingRegistry: true
]

{ #category : 'accessing' }
MetacelloScriptEngine >> loader [

	^ (self options
		   at: #loader
		   ifAbsentPut: [ MetacelloMonticelloLoader new ])
		  engine: self;
		  yourself
]

{ #category : 'actions api' }
MetacelloScriptEngine >> lock [

	^ MetacelloProjectRegistry
		  copyRegistryDuring: [
				  | spec |
				  spec := self projectSpec.
				  MetacelloProjectRegistry
					  registrationNamed: spec className
					  ifPresent: [ :existing |
							  existing locked: true. "We need to merge the repositories"
							  existing projectSpec repositories: spec repositories.
							  spec := existing projectSpec ]
					  ifAbsent: [
							  spec asProjectRegistration
								  locked: true;
								  registerProject ].
				  spec ]
		  commitIfSuccess: true
]

{ #category : 'accessing' }
MetacelloScriptEngine >> lookupProjectClassNamed: aString [ 
	
	^ self loader lookupProjectClassNamed: aString 
]

{ #category : 'accessing' }
MetacelloScriptEngine >> options [
    options ifNil: [ options := Dictionary new ].
    ^ options
]

{ #category : 'accessing' }
MetacelloScriptEngine >> options: aDictionary [
    options := aDictionary
]

{ #category : 'accessing' }
MetacelloScriptEngine >> projectSpec [

	^ projectSpec
]

{ #category : 'accessing' }
MetacelloScriptEngine >> projectSpec: aSpec [

	projectSpec := aSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> record [
	"Record the directive loading the default root, resolving first if needed (see ensureRecorded), and answer it"

	^ self during: [ self ensureRecorded ] keepingRegistry: false
]

{ #category : 'private' }
MetacelloScriptEngine >> recordDirective [
	"Record the resolution into a directive and answer it (see MetacelloRecordTarget>>#record:)"

	^ (MetacelloRecordTarget on: self) record: resolution
]

{ #category : 'actions api' }
MetacelloScriptEngine >> register [

	| spec |
	spec := self projectSpec.
	MetacelloProjectRegistry
		registrationNamed: spec className
		ifPresent: [ :existing | existing projectSpec: spec ]
		ifAbsent: [ spec asProjectRegistration registerProject ].
	^ spec
]

{ #category : 'resolution' }
MetacelloScriptEngine >> resolution [
	"The MetacelloResolution recording and loading follow"

	^ resolution
]

{ #category : 'resolution' }
MetacelloScriptEngine >> resolution: aResolution [
	"The resolution recording and loading follow from now on (see MetacelloResolveTarget)"

	resolution := aResolution
]

{ #category : 'actions api' }
MetacelloScriptEngine >> resolve [
	"Answer the resolution of a load of the default root, errors included, leaving the registry as it was"

	^ self during: [ self ensureResolved ] keepingRegistry: false
]

{ #category : 'private' }
MetacelloScriptEngine >> resolvePresentProject: existingRegistration new: newRegistration [

	existingRegistration locked ifTrue: [ ^ existingRegistration ].

	^ (existingRegistration hasLoadConflicts: newRegistration)
		  ifTrue: [
				  ((existingRegistration canUpgradeTo: newRegistration)
					   ifTrue: [ MetacelloAllowProjectUpgrade new ]
					   ifFalse: [ MetacelloAllowConflictingProjectUpgrade new ])
					  existingProjectRegistration: existingRegistration;
					  newProjectRegistration: newRegistration;
					  signal ]
		  ifFalse: [ newRegistration ]
]

{ #category : 'resolution' }
MetacelloScriptEngine >> resolvedProjectFor: aProjectSpec [
	"The project of the resolution aProjectSpec names. The resolver answered every project the load reaches: one missing is a bug of the resolver"

	^ resolution projectFor: aProjectSpec ifAbsent: [ MetacelloError signal: aProjectSpec name , ' is not in the resolution' ]
]

{ #category : 'resolution' }
MetacelloScriptEngine >> resolver [
	"The MetacelloResolver that chooses the version of each project, MetacelloInteractiveResolver unless the options name another"

	^ self options at: #resolver ifAbsent: [ MetacelloInteractiveResolver new ]
]

{ #category : 'accessing' }
MetacelloScriptEngine >> specBuilder [
	^ specBuilder
]

{ #category : 'accessing' }
MetacelloScriptEngine >> strict [
	
	^ self options at: #'strict' ifAbsent: [ false ]
]

{ #category : 'actions api' }
MetacelloScriptEngine >> unlock [

	^ MetacelloProjectRegistry
		  copyRegistryDuring: [
				  | spec |
				  spec := self projectSpec.
				  MetacelloProjectRegistry registrationNamed: spec className ifPresent: [ :existing | existing locked: false ].
				  spec ]
		  commitIfSuccess: true
]

{ #category : 'actions api' }
MetacelloScriptEngine >> unregister [

	| spec |
	spec := self projectSpec.
	MetacelloProjectRegistry registrationNamed: spec className ifPresent: [ :existing | existing unregisterProject ].
	^ spec
]
