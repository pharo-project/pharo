"
I am the record target: I turn the engine's resolution into load directives (record: answers the root directive). I walk from each root, in order, and record each package where it is first asked: a project reference records its project at that place, with the names it asks that are not recorded yet. A project's preLoad and postLoad go around its first packages only.
I decide no version: each project reference takes the project the resolution chose.
"
Class {
	#name : 'MetacelloRecordTarget',
	#superclass : 'MetacelloTarget',
	#instVars : [
		'required',
		'currentRepositorySpecs',
		'recorded',
		'hookedVersionSpecs'
	],
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'visiting' }
MetacelloRecordTarget >> atomicLoadPackages: packages ofSpec: spec [
	"Visit the packages inside a new atomic directive: they are loaded together"

	self specBuilder
		pushAtomicLoadDirectivesDuring: [
				self specBuilder currentLoadDirective spec: spec.
				packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'accessing' }
MetacelloRecordTarget >> hookedVersionSpecs [
	"The version specs whose preLoad and postLoad are recorded"

	^ hookedVersionSpecs ifNil: [ hookedVersionSpecs := IdentitySet new ]
]

{ #category : 'visiting' }
MetacelloRecordTarget >> linearLoadPackages: packages ofSpec: spec [
	"Visit the packages inside the root linear directive: each is loaded on its own, in order"

	self specBuilder
		pushLinearLoadDirectivesDuring: [
				self specBuilder currentLoadDirective spec: spec.
				packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'recording' }
MetacelloRecordTarget >> postLoad: packageOrVersionSpec [
	"Add the directive that runs the spec's postLoadDoIt (it does nothing when there is none)"

	(MetacelloPostLoadDirective spec: packageOrVersionSpec) addTo: self specBuilder currentLoadDirective
]

{ #category : 'visiting' }
MetacelloRecordTarget >> preLoad: packageOrVersionSpec [
	"Add the directive that runs the spec's preLoadDoIt (it does nothing when there is none)"

	(MetacelloPreLoadDirective spec: packageOrVersionSpec) addTo: self specBuilder currentLoadDirective
]

{ #category : 'recording' }
MetacelloRecordTarget >> record: aResolution [
	"The directive loading aResolution: its roots in their order, each package where it is first asked (see visitProjectReference:), each project's preLoad and postLoad around its first packages (see visitVersionSpec:)"

	aResolution roots do: [ :root | self recordProject: root loading: root loadList ].
	^ self specBuilder root
]

{ #category : 'recording' }
MetacelloRecordTarget >> record: aVersionSpec loading: aCollectionOfNames [
	"Add the packages aCollectionOfNames reach in aVersionSpec, in one directive (see visitVersionSpec:). A project reference among them records its project here, so required is restored afterwards"

	| oldRequired |
	oldRequired := required.
	required := aCollectionOfNames.
	[ aVersionSpec acceptVisitor: self ] ensure: [ required := oldRequired ]
]

{ #category : 'recording' }
MetacelloRecordTarget >> recordProject: aResolvedProject loading: aCollectionOfNames [
	"Add the names of aCollectionOfNames not recorded yet for aResolvedProject, at its resolved version: a project is recorded at each place that asks it something new, never twice for the same names. A locked project is left as the image has it"

	| names done |
	MetacelloNotification signal: 'Project: ' , aResolvedProject name.
	aResolvedProject isLocked ifTrue: [ ^ MetacelloNotification signal: 'Project is locked, skipping.' ].
	done := self recorded at: aResolvedProject ifAbsentPut: [ Set new ].
	names := (aCollectionOfNames reject: [ :each | done includes: each ]) asArray.
	names ifEmpty: [ ^ self ].
	done addAll: names.
	self record: aResolvedProject version loading: names
]

{ #category : 'accessing' }
MetacelloRecordTarget >> recorded [
	"The names recorded so far for each resolved project"

	^ recorded ifNil: [ recorded := Dictionary new ]
]

{ #category : 'accessing' }
MetacelloRecordTarget >> specBuilder [
	"The engine's builder of the directive: what I add goes to its current directive"

	^ engine specBuilder
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitGroupSpec: aMetacelloGroupSpec [
	"Nothing: a group only names packages, which packageSpecsInLoadOrderFor: already expanded"
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitPackageSpec: aMetacelloPackageSpec [
	"Create the directives of a package: its preLoad, the package, its postLoad; nothing when the package is in the directive already"

	| directive packageRepositorySpecs |
	packageRepositorySpecs := aMetacelloPackageSpec repositorySpecs ifEmpty: [ currentRepositorySpecs ].
	directive := (MetacelloPackageLoadDirective spec: aMetacelloPackageSpec)
		             repositorySpecs: packageRepositorySpecs;
		             yourself.
	(self specBuilder currentLoadDirective includes: directive) ifTrue: [ ^ self ].
	self preLoad: aMetacelloPackageSpec.
	directive addTo: self specBuilder currentLoadDirective.
	self postLoad: aMetacelloPackageSpec
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectReference: aMetacelloProjectReferenceSpec [
	"Record the referenced project here, at the version the resolution chose, with what this reference asks of it: a group asked later is loaded where it is asked (BaselineOfPharo asks Monticello's RemoteRepositories early and BaselineOfIDE its Tests once SUnit is loaded). A locked project is left as the image has it"

	| reference project |
	reference := aMetacelloProjectReferenceSpec projectReference.
	reference isLocked ifTrue: [
		MetacelloNotification signal: 'Project: ' , reference name.
		^ MetacelloNotification signal: 'Project is locked, skipping.' ].
	project := engine resolvedProjectFor: reference.
	self recordProject: project loading: (reference loadListForVersion: project version)
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpec: aMetacelloVersionSpec [
	"Create the directives of the packages required reaches. The first time a version spec is visited, its preLoad and postLoad go around its packages; when it is visited again (a project asked in several places) they do not run again"

	(self hookedVersionSpecs includes: aMetacelloVersionSpec) ifTrue: [ ^ self visitVersionSpecChildren: aMetacelloVersionSpec ].
	self hookedVersionSpecs add: aMetacelloVersionSpec.
	self preLoad: aMetacelloVersionSpec.
	self visitVersionSpecChildren: aMetacelloVersionSpec.
	self postLoad: aMetacelloVersionSpec
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpecChildren: aVersionSpec [
	"Create the directives of the packages the names in required reach, in load order, inside one linear or atomic directive as the project says, with the version spec's repositories"

	| packages oldCurrentRepositorySpecs |
	oldCurrentRepositorySpecs := currentRepositorySpecs.
	currentRepositorySpecs := engine loader resolveRepositorySpecs: aVersionSpec repositorySpecs.
	packages := aVersionSpec packageSpecsInLoadOrderFor: required.
	[ "Iterate the internals of the version spec"
		aVersionSpec project loadType == #atomic ifTrue: [ ^ self atomicLoadPackages: packages ofSpec: aVersionSpec ].
		aVersionSpec project loadType == #linear ifTrue: [ ^ self linearLoadPackages: packages ofSpec: aVersionSpec ].
		MetacelloError signal: 'Unknown loadType: ' , aVersionSpec project loadType printString ] ensure: [
		currentRepositorySpecs := oldCurrentRepositorySpecs ]
]
