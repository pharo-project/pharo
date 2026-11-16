"
I am Metacello's default resolver: a project takes the version its dependents declare, and whenever that conflicts with what is already registered (an upgrade, a downgrade, a conflict, a lock), I ask: I signal a notification that the script's handlers (onUpgrade:, onDowngrade:, onConflict:, onLock:) or the user answer. I load the baseline of each project I choose, and register it.

engine: the MetacelloScriptEngine resolving, for its loader, options and handlers.
resolution: the MetacelloResolution being built.
requester: the name of the project whose references I am visiting.
path: the projects whose references I am visiting, the outermost first: a project reached again on it is a dependency cycle.
"
Class {
	#name : 'MetacelloInteractiveResolver',
	#superclass : 'MetacelloResolver',
	#instVars : [
		'engine',
		'resolution',
		'requester',
		'path'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'resolving' }
MetacelloInteractiveResolver >> addRootFor: aProjectSpec [
	"Add the root aProjectSpec names to the resolution, at the version it asks, loading what it loads, and answer it. It takes the version asked over the registry when that is an upgrade, and always when aProjectSpec is in dev mode"

	| rootSpec root |
	[
		[ rootSpec := (self registrationFor: aProjectSpec) projectSpec copy ]
			on: MetacelloAllowProjectUpgrade
			do: [ :notification | notification resume: notification newProjectRegistration ] ]
		on: MetacelloAllowConflictingProjectUpgrade
		do: [ :notification |
				aProjectSpec isDev
					ifTrue: [ notification resume: notification newProjectRegistration ]
					ifFalse: [ notification pass ] ].
	"The root keeps the dev mark of the spec asked: references to its project look for it (see devRootFor:ifFound:)"
	aProjectSpec isDev ifTrue: [ rootSpec beDev ].
	rootSpec ensureProjectLoadedWithEngine: engine.
	root := MetacelloResolvedProject projectSpec: rootSpec.
	resolution addRoot: root.
	root requestedBy: MetacelloUserRequest new spec: (rootSpec copy
			 loads: aProjectSpec loads;
			 yourself).
	^ root
]

{ #category : 'visiting' }
MetacelloInteractiveResolver >> devRootFor: aProjectSpec ifFound: aBlock [
	"Give aBlock the root in dev mode of the project aProjectSpec asks for, when there is one. The roots are added before any reference is visited, each keeping the dev mark of the spec the script gave (see addRootFor:), so the resolution itself knows them: no separate list of dev projects is kept"

	| project |
	project := resolution projectFor: aProjectSpec ifAbsent: [ ^ self ].
	project projectSpec isDev ifTrue: [ aBlock value: project ]
]

{ #category : 'private' }
MetacelloInteractiveResolver >> path [
	"The projects whose references are being visited, the outermost first"

	^ path ifNil: [ path := OrderedCollection new ]
]

{ #category : 'private' }
MetacelloInteractiveResolver >> prepare: aProjectSpec registering: aRegistration [
	"Read the baseline of the version chosen, give it the strictness its dependent declared, and register it, so a later request is compared with it"

	aProjectSpec ensureProjectLoadedWithEngine: engine.
	aProjectSpec project version strict: aProjectSpec strict.
	aRegistration registerProject
]

{ #category : 'private' }
MetacelloInteractiveResolver >> registrationFor: aProjectSpec [
	"A project already registered is reconciled with aProjectSpec (locks, upgrades, downgrades and conflicts, answered by the engine's handlers); any other is taken as declared"

	^ MetacelloProjectRegistry
		  registrationNamed: aProjectSpec className
		  ifPresent: [ :existing | engine resolvePresentProject: existing new: aProjectSpec asProjectRegistration ]
		  ifAbsent: [ aProjectSpec asProjectRegistration ]
]

{ #category : 'private' }
MetacelloInteractiveResolver >> reportCycleAt: aResolvedProject [
	"aResolvedProject requires itself through the projects after it on the path: report the cycle on its last request"

	| cycle |
	cycle := (path copyFrom: (path indexOf: aResolvedProject) to: path size) copyWith: aResolvedProject.
	resolution addFinding: (MetacelloResolutionFinding
			 error: #cycle
			 text: 'Dependency cycle: ' , (' -> ' join: (cycle collect: [ :each | each name ]))
			 request: aResolvedProject requests last)
]

{ #category : 'resolving' }
MetacelloInteractiveResolver >> resolveRoots: someProjectSpecs engine: anEngine [
	"Add a root for each of someProjectSpecs, in order, then walk the references of each. A root takes the version asked, even over the registry (an upgrade of a root is always allowed). A root marked dev is never replaced: every reference to its project takes it (see visitProjectReference:)"

	| roots |
	self setEngine: anEngine.
	roots := someProjectSpecs collect: [ :each | self addRootFor: each ].
	roots do: [ :each | self visitReferencesOf: each ].
	^ resolution
]

{ #category : 'private' }
MetacelloInteractiveResolver >> setEngine: anEngine [

	engine := anEngine.
	resolution := MetacelloResolution new
]

{ #category : 'visiting' }
MetacelloInteractiveResolver >> visitDevReference: aProjectSpec of: aRoot [
	"aProjectSpec asks for a project in dev mode, whose root is aRoot: it takes the root, at the root's version, whatever version it asks and without asking the handlers, and adds the groups it asks; the root's references are visited again when its groups grew"

	| loadsBefore |
	loadsBefore := aRoot loadList asSet.
	aRoot requestedBy: requester spec: (aProjectSpec copy
			 loads: (aProjectSpec loadListForVersion: aRoot version);
			 yourself).
	aRoot loadList asSet ~= loadsBefore ifTrue: [ self visitReferencesOf: aRoot ]
]

{ #category : 'visiting' }
MetacelloInteractiveResolver >> visitGroupSpec: aGroupSpec [
	"Groups need no version"

	
]

{ #category : 'visiting' }
MetacelloInteractiveResolver >> visitPackageSpec: aPackageSpec [
	"Packages need no version"

	
]

{ #category : 'visiting' }
MetacelloInteractiveResolver >> visitProjectReference: aProjectReferenceSpec [
	"A locked project is left as it is, and a project in dev mode keeps its root (see visitDevReference:of:). Otherwise the registry decides between the version registered and the one declared (the script's handlers answer upgrades, downgrades and conflicts). A project already resolved gets the groups asked added and, when the handlers chose another version, takes it; its references are visited again when its version or its groups changed"

	| reference registration registered toLoad project loadsBefore changed |
	reference := aProjectReferenceSpec projectReference.
	reference isLocked ifTrue: [ ^ self ].
	self devRootFor: reference ifFound: [ :root | ^ self visitDevReference: reference of: root ].

	registration := self registrationFor: reference.
	registered := registration projectSpec.
	toLoad := (registered compareEqual: reference)
		          ifTrue: [ reference ]
		          ifFalse: [
				          registered copy
					          mergeScriptLoads: reference;
					          yourself ].

	changed := false.
	project := resolution projectFor: toLoad ifAbsent: [ nil ].
	project
		ifNil: [
				self prepare: toLoad registering: registration.
				project := resolution add: (MetacelloResolvedProject projectSpec: toLoad) ]
		ifNotNil: [
				(project projectSpec hasLoadConflicts: toLoad) ifTrue: [
						self prepare: toLoad registering: registration.
						project changeVersionTo: toLoad.
						changed := true ] ].
	loadsBefore := project loadList asSet.
	project requestedBy: requester spec: (reference copy
			 loads: (registered loadListForVersion: project version);
			 yourself).
	(changed or: [ project loadList asSet ~= loadsBefore ]) ifTrue: [ self visitReferencesOf: project ]
]

{ #category : 'visiting' }
MetacelloInteractiveResolver >> visitProjectSpec: aProjectSpec [
	"Only project references take part in resolution"

	
]

{ #category : 'private' }
MetacelloInteractiveResolver >> visitReferencesOf: aResolvedProject [
	"Visit the references of aResolvedProject, unless it is already on the path being visited: a project requiring itself through others cannot be loaded in any order, an error of the resolution"

	(self path includes: aResolvedProject) ifTrue: [ ^ self reportCycleAt: aResolvedProject ].
	self path addLast: aResolvedProject.
	[ self visitReferencesOf: aResolvedProject version loads: aResolvedProject loadList requester: aResolvedProject ] ensure: [
		self path removeLast ]
]

{ #category : 'private' }
MetacelloInteractiveResolver >> visitReferencesOf: aVersionSpec loads: aCollectionOfNames requester: aResolvedProjectOrNil [
	"Resolve the projects loading aCollectionOfNames from aVersionSpec reaches, as asked by aResolvedProjectOrNil"

	| previousRequester |
	previousRequester := requester.
	requester := aResolvedProjectOrNil.
	[ (aVersionSpec packageSpecsInLoadOrderFor: aCollectionOfNames) do: [ :each | each acceptVisitor: self ] ] ensure: [
		requester := previousRequester ]
]
