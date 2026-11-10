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

{ #category : 'private' }
MetacelloRecordTarget >> atomicLoadPackages: packages ofSpec: spec [

	self specBuilder
		pushAtomicLoadDirectivesDuring: [
			self specBuilder currentLoadDirective spec: spec.
			packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'private' }
MetacelloRecordTarget >> linearLoadPackages: packages ofSpec: spec [

	self specBuilder
		pushLinearLoadDirectivesDuring: [
			self specBuilder currentLoadDirective spec: spec.
			packages do: [ :pkg | pkg acceptVisitor: self ] ]
		for: self
]

{ #category : 'do-its' }
MetacelloRecordTarget >> postLoad: packageOrVersionSpec [

	(MetacelloPostLoadDirective spec: packageOrVersionSpec) addTo: self specBuilder currentLoadDirective
]

{ #category : 'do-its' }
MetacelloRecordTarget >> preLoad: packageOrVersionSpec [

	(MetacelloPreLoadDirective spec: packageOrVersionSpec) addTo: self specBuilder currentLoadDirective
]

{ #category : 'recording' }
MetacelloRecordTarget >> record: aVersionSpec [
	"Record the load directives for what I am required to load from aVersionSpec, and answer the root directive. Recording takes each project from the engine's resolution: a script resolves first (see MetacelloScriptEngine>>#do:required:commitIfSuccess:); a version spec recorded on its own is resolved here"

	engine resolution ifNil: [ engine resolution: ((MetacelloResolveTarget on: engine) resolveVersion: aVersionSpec loads: required) ].
	aVersionSpec acceptVisitor: self.
	^ self specBuilder root
]

{ #category : 'accessing' }
MetacelloRecordTarget >> required: aCollection [ 
	
	required := aCollection
]

{ #category : 'private' }
MetacelloRecordTarget >> specBuilder [
		
	^ engine specBuilder
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitGroupSpec: aMetacelloGroupSpec [ 
	
	"Nothing"
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitPackageSpec: aMetacelloPackageSpec [

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
	"Record the project at the version the resolution chose, here with the groups this reference asks: a group asked later is loaded where it is asked, after what comes before it (BaselineOfPharo asks Monticello's RemoteRepositories early and BaselineOfIDE its Tests once SUnit is loaded)"

	| projectReferenceToLoad project names |
	projectReferenceToLoad := aMetacelloProjectReferenceSpec projectReference.
	MetacelloNotification signal: 'Project: ' , projectReferenceToLoad name.
	projectReferenceToLoad isLocked ifTrue: [
			MetacelloNotification signal: 'Project is locked, skipping.'.
			^ projectReferenceToLoad ].
	project := engine resolvedProjectFor: projectReferenceToLoad.
	names := engine namesToRecord: (projectReferenceToLoad loadListForVersion: project version) of: project.
	names ifEmpty: [ ^ self ].
	project version recordRequiredFromArray: names withEngine: engine
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitProjectSpec: aMetacelloProjectSpec [
	
	"Download the baseline!"
	| loadedSpec |
	loadedSpec := engine lookupProjectSpecFor: aMetacelloProjectSpec.

	"And interpret"
	currentRepositorySpecs := aMetacelloProjectSpec repositorySpecs.
	loadedSpec projectPackage acceptVisitor: self
]

{ #category : 'visiting' }
MetacelloRecordTarget >> visitVersionSpec: aMetacelloVersionSpec [

	self preLoad: aMetacelloVersionSpec.
	self visitVersionSpecChildren: aMetacelloVersionSpec.
	self postLoad: aMetacelloVersionSpec
]

{ #category : 'actions' }
MetacelloRecordTarget >> visitVersionSpecChildren: aVersionSpec [

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
