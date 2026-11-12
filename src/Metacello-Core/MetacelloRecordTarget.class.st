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
	"Add one package: its preLoad, the package, its postLoad. A package already in the directive is skipped"

	| directive packageRepositorySpecs |
	"The package's own repositories, or those of its version spec"
	packageRepositorySpecs := aMetacelloPackageSpec repositorySpecs ifEmpty: [ currentRepositorySpecs ].
	directive := (MetacelloPackageLoadDirective spec: aMetacelloPackageSpec)
		             repositorySpecs: packageRepositorySpecs;
		             yourself.
	"Already there: nothing to add"
	(self specBuilder currentLoadDirective includes: directive) ifTrue: [ ^ self ].
	self preLoad: aMetacelloPackageSpec.
	directive addTo: self specBuilder currentLoadDirective.
	self postLoad: aMetacelloPackageSpec
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectReference: aMetacelloProjectReferenceSpec [
	"Reached when a version spec's packages include a reference to another project (spec baseline: 'Bloc' with: [...]): the visitor sends this for it. Nothing to do: that project is a project of the resolution, already added to the directive before this one (see record:). Kept because the visitor sends it"
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectSpec: aMetacelloProjectSpec [
	"Not reached by recording a load (version specs are visited, see recordProjects:); kept from before: look the project's baseline up and visit its package"

	| loadedSpec |
	loadedSpec := engine lookupProjectSpecFor: aMetacelloProjectSpec.
	currentRepositorySpecs := aMetacelloProjectSpec repositorySpecs.
	loadedSpec projectPackage acceptVisitor: self
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpec: aMetacelloVersionSpec [
	"Add one version spec to the directive: its preLoad, then its packages, then its postLoad. Sent by acceptVisitor: from record: and recordProjects:"

	"Its preLoad first (a directive that runs preLoadDoIt, if it has one)"
	self preLoad: aMetacelloVersionSpec.
	"Then its packages, for the names in required"
	self visitVersionSpecChildren: aMetacelloVersionSpec.
	"Its postLoad last"
	self postLoad: aMetacelloVersionSpec
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpecChildren: aVersionSpec [
	"Add the packages of aVersionSpec that the names in required reach, in their load order, inside one linear or atomic directive as its project says"

	| packages oldCurrentRepositorySpecs |
	"Remember the repositories the packages come from, and use aVersionSpec's while visiting its packages"
	oldCurrentRepositorySpecs := currentRepositorySpecs.
	currentRepositorySpecs := engine loader resolveRepositorySpecs: aVersionSpec repositorySpecs.
	"The package and project specs required reaches, in load order (a project reference among them goes to visitProjectReference:, which adds nothing)"
	packages := aVersionSpec packageSpecsInLoadOrderFor: required.
	[ "Visit them inside a directive of the project's load type"
		aVersionSpec project loadType == #atomic ifTrue: [ ^ self atomicLoadPackages: packages ofSpec: aVersionSpec ].
		aVersionSpec project loadType == #linear ifTrue: [ ^ self linearLoadPackages: packages ofSpec: aVersionSpec ].
		MetacelloError signal: 'Unknown loadType: ' , aVersionSpec project loadType printString ] ensure: [
		"Back to the repositories of before"
		currentRepositorySpecs := oldCurrentRepositorySpecs ]
]
