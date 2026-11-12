"
MetacelloScriptEngine runs the execution of the script for one projectSpec
"
Class {
	#name : 'MetacelloScriptEngine',
	#superclass : 'Object',
	#instVars : [
		'root',
		'options',
		'specBuilder',
		'projectSpec',
		'resolution',
		'version'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'private' }
MetacelloScriptEngine >> directiveLoading: required [
	"The directive loading the resolution: every project once, after the projects it requires. A version spec loaded on its own (no script) is not a project of the resolution: it comes last, after the projects it requires, with the names required of it"

	| target |
	target := MetacelloRecordTarget on: self.
	version ifNil: [ ^ target record: resolution ].
	target recordProjects: (resolution loadOrderFrom: (resolution requirementsOf: nil in: version)).
	target record: version loading: (self versionLoads: required).
	^ target specBuilder root
]

{ #category : 'private' }
MetacelloScriptEngine >> do: aBlock required: required commitIfSuccess: aBoolean [
	"Resolve first; with errors, signal MetacelloResolutionError and do nothing. Otherwise record the resolution into one directive (see directiveLoading:) and run aBlock on it; its answer is the root of this engine"

	MetacelloProjectRegistry
		copyRegistryDuring: [
				self handleNotificationsForAction: [
						self resolve: required.
						resolution hasErrors ifTrue: [ MetacelloResolutionError signalFor: resolution ].
						self root: (aBlock value: (self directiveLoading: required)) ] ]
		commitIfSuccess: aBoolean
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> doLoadProjectSpecFrom: aMetacelloPackageSpec [

	self loader
		loadAtomicPackageDirectives: { (MetacelloPackageLoadDirective spec: aMetacelloPackageSpec) }
		engine: self
]

{ #category : 'actions api' }
MetacelloScriptEngine >> fetch: required [
	^ self
		  do: [ :directive |
				  directive acceptVisitor: (MetacelloFetchTarget on: self).
				  directive ]
		  required: required
		  commitIfSuccess: false
]

{ #category : 'actions api' }
MetacelloScriptEngine >> get [
	" load a fresh copy from repo"

	MetacelloProjectRegistry
		copyRegistryDuring: [
				self handleNotificationsForAction: [
						| spec projectPackage |
						spec := self projectSpec.
						projectPackage := spec projectPackage.
						projectPackage repositorySpecs do: [ :repoSpec | repoSpec createRepository flushForScriptGet ].
						self doLoadProjectSpecFrom: projectPackage.
						self root: (Smalltalk at: spec className asSymbol) project.
						MetacelloProjectRegistry
							registrationNamed: spec className
							ifPresent: [ :existing | existing projectSpec repositories: spec repositories ]
							ifAbsent: [ spec asProjectRegistration registerProject ] ] ]
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

	[
		[
			actionBlock
				on: MetacelloAllowProjectUpgrade , MetacelloAllowConflictingProjectUpgrade
				do: [ :ex | "option handlers need to be outermost set of handlers ... last line of defense before users are involved" ex handleResolutionFor: self ] ]
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

	self root: self projectSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> load: required [
	self
		do: [ :directive |
				directive acceptVisitor: (MetacelloFetchTarget on: self).
				(MetacelloLoadTarget on: self) load: directive.
				directive ]
		required: required
		commitIfSuccess: true
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

	| spec |
	MetacelloProjectRegistry
		copyRegistryDuring: [
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
				self root: spec ]
		commitIfSuccess: true
]

{ #category : 'accessing' }
MetacelloScriptEngine >> lookupProjectClassNamed: aString [ 
	
	^ self loader lookupProjectClassNamed: aString 
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> lookupProjectSpecFor: aProjectSpec [
	"if there is no conflict, choose new spec"

	^ MetacelloProjectRegistry
		  registrationNamed: aProjectSpec className
		  ifPresent: [ :existing | (self resolvePresentProject: existing new: aProjectSpec asProjectRegistration) projectSpec ]
		  ifAbsent: [ aProjectSpec ]
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
MetacelloScriptEngine >> record: required [
	^ self
		  do: [ :directive | directive ]
		  required: required
		  commitIfSuccess: false
]

{ #category : 'actions api' }
MetacelloScriptEngine >> register [

	| spec |
	spec := self projectSpec.
	MetacelloProjectRegistry
		registrationNamed: spec className
		ifPresent: [ :existing | existing projectSpec: spec ]
		ifAbsent: [ spec asProjectRegistration registerProject ].
	self root: spec
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
MetacelloScriptEngine >> resolve: required [
	"Answer the resolution of a load of required, errors included, leaving the registry as it was: from the root, or from a version spec loaded on its own (no script, see version:)"

	MetacelloProjectRegistry
		copyRegistryDuring: [
				self handleNotificationsForAction: [
						self resolution: (version
								 ifNil: [ (MetacelloResolveTarget on: self) resolve: self projectSpec loads: required ]
								 ifNotNil: [ (MetacelloResolveTarget on: self) resolveVersion: version loads: (self versionLoads: required) ]).
						self root: resolution ] ]
		commitIfSuccess: false.
	^ resolution
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
MetacelloScriptEngine >> root [
	^ root
]

{ #category : 'accessing' }
MetacelloScriptEngine >> root: anObject [
	root := anObject
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

	| spec |
	MetacelloProjectRegistry
		copyRegistryDuring: [
				spec := self projectSpec.
				MetacelloProjectRegistry registrationNamed: spec className ifPresent: [ :existing | existing locked: false ].
				self root: spec ]
		commitIfSuccess: true
]

{ #category : 'actions api' }
MetacelloScriptEngine >> unregister [

	| spec |
	spec := self projectSpec.
	MetacelloProjectRegistry registrationNamed: spec className ifPresent: [ :existing | existing unregisterProject ].
	self root: spec
]

{ #category : 'accessing' }
MetacelloScriptEngine >> version: aVersionSpec [
	"Resolve from aVersionSpec, loaded on its own (no script), instead of from the root"

	version := aVersionSpec
]

{ #category : 'private' }
MetacelloScriptEngine >> versionLoads: required [
	"The names to load from a version spec loaded on its own: required, or its default ones when none"

	^ required ifEmpty: [ version defaultPackageNames ]
]
