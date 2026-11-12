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
		'directive',
		'fetched'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'private' }
MetacelloScriptEngine >> doLoad: rootLoadList [
	"The directive, loaded, fetching first if needed"

	(MetacelloLoadTarget on: self) load: (self ensureFetched: rootLoadList).
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
	"Run aBlock on a copy of the registry, with the script's handlers for upgrades, downgrades, conflicts and locks; keep the copy only when aBlock succeeds and aBoolean"

	MetacelloProjectRegistry
		copyRegistryDuring: [ self handleNotificationsForAction: aBlock ]
		commitIfSuccess: aBoolean
]

{ #category : 'private' }
MetacelloScriptEngine >> ensureFetched: rootLoadList [
	"The directive, its packages fetched once, recording first if needed"

	fetched == true ifTrue: [ ^ directive ].
	(self ensureRecorded: rootLoadList) acceptVisitor: (MetacelloFetchTarget on: self).
	fetched := true.
	^ directive
]

{ #category : 'private' }
MetacelloScriptEngine >> ensureRecorded: rootLoadList [
	"The directive loading the resolution, recorded once, resolving first if needed. A resolution with errors is never recorded: it signals MetacelloResolutionError"

	directive ifNotNil: [ ^ directive ].
	(self ensureResolved: rootLoadList) hasErrors ifTrue: [ MetacelloResolutionError signalFor: resolution ].
	^ directive := self recordDirective: rootLoadList
]

{ #category : 'private' }
MetacelloScriptEngine >> ensureResolved: rootLoadList [
	"The resolution of rootLoadList, resolved once from the root"

	resolution ifNil: [ self resolution: ((MetacelloResolveTarget on: self) resolve: self projectSpec loads: rootLoadList) ].
	^ resolution
]

{ #category : 'actions api' }
MetacelloScriptEngine >> fetch: rootLoadList [
	"Fetch what the directive loads, recording first if needed (see ensureFetched:), and answer the directive"

	self during: [ self root: (self ensureFetched: rootLoadList) ] keepingRegistry: false.
	^ root
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
MetacelloScriptEngine >> load: rootLoadList [
	"Load the directive, fetching first if needed (see doLoad:), and keep the registry changes"

	self during: [ self root: (self doLoad: rootLoadList) ] keepingRegistry: true.
	^ root
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
MetacelloScriptEngine >> record: rootLoadList [
	"Record the directive loading rootLoadList, resolving first if needed (see ensureRecorded:), and answer it"

	self during: [ self root: (self ensureRecorded: rootLoadList) ] keepingRegistry: false.
	^ root
]

{ #category : 'private' }
MetacelloScriptEngine >> recordDirective: rootLoadList [
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
MetacelloScriptEngine >> resolve: rootLoadList [
	"Answer the resolution of a load of rootLoadList (the names to load from the root, empty for its default ones), errors included, leaving the registry as it was"

	self during: [ self root: (self ensureResolved: rootLoadList) ] keepingRegistry: false.
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
