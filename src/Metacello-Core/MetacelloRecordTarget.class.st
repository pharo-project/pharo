"
I am the record target: I walk a version spec for the names I am required to load (required:) and record the load directives, packages and projects in load order, with their pre and post load do-its (record: answers the root directive).

I decide no version: each project reference takes the project the engine's resolution chose, recorded once with every group asked. A version spec recorded on its own is resolved first (see record:).
"
Class {
	#name : 'MetacelloRecordTarget',
	#superclass : 'MetacelloTarget',
	#instVars : [
		'required',
		'currentRepositorySpecs'
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

{ #category : 'visiting' }
MetacelloRecordTarget >> linearLoadPackages: packages ofSpec: spec [
	"Visit the packages inside the root linear directive: each is loaded on its own, in order"

	self specBuilder
		pushLinearLoadDirectivesDuring: [
				self specBuilder currentLoadDirective spec: spec.
				packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'visiting' }
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
	"The directive loading aResolution: every project once, after the projects it requires (see MetacelloResolution>>#loadOrder)"

	^ self recordProjects: aResolution loadOrder
]

{ #category : 'recording' }
MetacelloRecordTarget >> record: aVersionSpec loading: aCollectionOfNames [
	"Add aVersionSpec's preLoad, the packages aCollectionOfNames reach, and its postLoad (see visitVersionSpec:)"

	required := aCollectionOfNames.
	aVersionSpec acceptVisitor: self
]

{ #category : 'recording' }
MetacelloRecordTarget >> recordProjects: aCollectionOfResolvedProjects [
	"Add each project to the directive, in the order given, with every group asked of it, and answer the directive. A locked project is left as the image has it"

	aCollectionOfResolvedProjects do: [ :project |
			MetacelloNotification signal: 'Project: ' , project name.
			project isLocked
				ifTrue: [ MetacelloNotification signal: 'Project is locked, skipping.' ]
				ifFalse: [ self record: project version loading: project loadList ] ].
	^ self specBuilder root
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
	"Nothing to record. The packages of a version spec include references to the projects it requires (spec baseline: 'Bloc' with: [ ... ]), and visitVersionSpecChildren: sends acceptVisitor: to each, so every reference comes here; without this method recording would fail on the first baseline with a dependency. The project referenced is a project of the resolution: record: added it to the directive already, before this one, since the load order puts every project after the projects it requires"
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpec: aMetacelloVersionSpec [
	"Create the directives of a version spec: its preLoad, its packages (see visitVersionSpecChildren:), its postLoad"

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
