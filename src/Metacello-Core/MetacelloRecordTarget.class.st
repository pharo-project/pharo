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

{ #category : 'actions' }
MetacelloRecordTarget >> listLoadableSpecsFrom: aVersionSpec [
	"I take a MetacelloVersionSpec and I return all the specs that needs to be loaded from my requirements."

	| loadableSpecs requirementList managedRequirements |
	loadableSpecs := OrderedCollection new.
	requirementList := required copy.
	managedRequirements := Set new.
	[ requirementList isEmpty ] whileFalse: [
			| newReqd |
			newReqd := Set new.
			requirementList do: [ :req |
					| loadableSpec |
					loadableSpec := aVersionSpec packageNamed: req ifAbsent: [ 
						^ self error: 'Name not found: ' , req, ' in ', aVersionSpec label ]. "If the loadable spec is a Group we ignore it."
					loadableSpec resolveToLoadableSpec ifNotNil: [ :resolvedSpec | loadableSpecs add: resolvedSpec ].
					newReqd addAll: loadableSpec requires.
					newReqd addAll: loadableSpec includes ].
			managedRequirements addAll: requirementList.
			newReqd removeAllFoundIn: managedRequirements.
			requirementList := newReqd ].
	^ loadableSpecs
]

{ #category : 'actions' }
MetacelloRecordTarget >> packageSpecsInLoadOrderFrom: aVersionSpec [

	| loadOrder specsToLoad packageNames importNames importSpec importProjectSpecs importProjectNameMap |
	loadOrder := aVersionSpec packageSpecsInLoadOrder.
	importNames := (packageNames := ((self listLoadableSpecsFrom: aVersionSpec) collect: [ :spec | spec name ]) asSet) copy.

	aVersionSpec hasImports ifFalse: [ ^ loadOrder select: [ :spec | packageNames includes: spec name ] ].

	loadOrder do: [ :spec | importNames remove: spec name ifAbsent: [ ] ].
	specsToLoad := OrderedCollection new.
	importProjectSpecs := Dictionary new.
	importProjectNameMap := Dictionary new.
	aVersionSpec importArray ifNotNil: [ :importArray |
			loadOrder do: [ :spec |
					importArray do: [ :assoc |
							assoc key = spec name ifTrue: [
									importProjectSpecs at: spec name put: spec.
									(assoc value select: [ :each | importNames includes: each ]) do: [ :each | (importProjectNameMap at: spec name ifAbsentPut: [ Set new ]) add: each ] ] ] ] ].

	aVersionSpec import ifNotNil: [ :import |
			loadOrder do: [ :spec |
					spec name = import ifTrue: [
							importProjectSpecs at: spec name put: spec.
							importProjectNameMap at: spec name put: importNames ] ] ].

	loadOrder do: [ :spec |
			(packageNames includes: spec name) ifTrue: [ specsToLoad add: spec ].
			importProjectSpecs at: spec name ifPresent: [ :importProjectSpec | "insert the imports at this point"
					importProjectNameMap at: spec name ifPresent: [ :importedNames |
							importedNames do: [ :importedName |
									specsToLoad add: (importSpec := importProjectSpec copy
											               name: importedName;
											               mergeImportLoads: { importedName };
											               yourself).
									importSpec projectReference name: importedName ] ] ] ].
	^ specsToLoad
]

{ #category : 'doits' }
MetacelloRecordTarget >> postLoad: packageOrVersionSpec [

	(MetacelloPostLoadDirective spec: packageOrVersionSpec) addTo: self specBuilder currentLoadDirective
]

{ #category : 'doits' }
MetacelloRecordTarget >> preLoad: packageOrVersionSpec [

	(MetacelloPreLoadDirective spec: packageOrVersionSpec) addTo: self specBuilder currentLoadDirective
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

	| registered override registration requiredArray projectReferenceToLoad |
	projectReferenceToLoad := aMetacelloProjectReferenceSpec projectReference.

	MetacelloNotification signal: 'Project: ' , projectReferenceToLoad name.

	projectReferenceToLoad isLocked ifTrue: [
			MetacelloNotification signal: 'Project is locked, skipping.'.
			^ projectReferenceToLoad ].

	registration := MetacelloProjectRegistry
		                registrationNamed: projectReferenceToLoad className
		                ifPresent: [ :existing | engine resolvePresentProject: existing new: projectReferenceToLoad asProjectRegistration ]
		                ifAbsent: [ projectReferenceToLoad asProjectRegistration ].
	registered := registration projectSpec.

	(registered compareEqual: projectReferenceToLoad) ifFalse: [ "counts as override, only if they differ in some aspect"
		override := registered copy.
		override mergeScriptLoads: projectReferenceToLoad.
		projectReferenceToLoad := override ].

	projectReferenceToLoad ensureProjectLoadedWithEngine: engine.
	"Propagate the strictness to the dependency as defined in the dependent before loading"
	projectReferenceToLoad project version strict: projectReferenceToLoad strict.
	
	requiredArray := registered loadListForVersion: projectReferenceToLoad version.
	projectReferenceToLoad version
		recordRequiredFromArray: requiredArray
		withEngine: engine.
	registration registerProject
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

	packages := self packageSpecsInLoadOrderFrom: aVersionSpec.

	[ "Iterate the internals of the version spec"
		aVersionSpec project loadType == #atomic ifTrue: [ ^ self atomicLoadPackages: packages ofSpec: aVersionSpec ].
		aVersionSpec project loadType == #linear ifTrue: [ ^ self linearLoadPackages: packages ofSpec: aVersionSpec ].

		MetacelloError signal: 'Unknown loadType: ' , aVersionSpec project loadType printString ] ensure: [ currentRepositorySpecs := oldCurrentRepositorySpecs ]
]
