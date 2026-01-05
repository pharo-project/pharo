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
		'projectReferenceSpec'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'utilities' }
MetacelloScriptEngine class >> baseNameOf: className [

	^ className withoutPrefix: 'BaselineOf'
]

{ #category : 'actions api' }
MetacelloScriptEngine >> do: aBlock required: required commitIfSuccess: aBoolean [

	MetacelloProjectRegistration
		copyRegistryDuring: [
				self handleNotificationsForAction: [
						| versionSpec projectSpec requiredArray |
						self validateProjectSpec.
						[ projectSpec := (self lookupProjectSpecFor: self projectReferenceSpec) copy ]
							on: MetacelloAllowProjectUpgrade
							do: [ :notification | notification resume: notification newProjectRegistration ].

						projectSpec loads: required.
						projectSpec ensureProjectLoadedWithEngine: self.
						versionSpec := projectSpec project version.
						requiredArray := projectSpec loadListForVersion: versionSpec.
						self root: (aBlock value: versionSpec value: requiredArray value: projectSpec) ] ]
		commitIfSuccess: aBoolean
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> doLoadProjectSpecFrom: aMetacelloPackageSpec [

	self loader loadPackageDirective: (MetacelloDirective
			 loadPackage: aMetacelloPackageSpec
			 repositorySpecs: aMetacelloPackageSpec repositorySpecs)
]

{ #category : 'actions api' }
MetacelloScriptEngine >> fetch: required [

	^ self
		  do: [ :versionSpec :requiredArray :projectSpec | versionSpec doFetchRequiredFromArray: requiredArray withEngine: self ]
		  required: required
		  commitIfSuccess: false
]

{ #category : 'actions api' }
MetacelloScriptEngine >> get [
	" load a fresh copy from repo"

	MetacelloProjectRegistration
		copyRegistryDuring: [
				self handleNotificationsForAction: [
						| spec projectPackage |
						self validateProjectSpec.
						spec := self projectReferenceSpec.
						projectPackage := spec projectPackage.
						projectPackage repositorySpecs do: [ :repoSpec | repoSpec createRepository flushForScriptGet ].
						self doLoadProjectSpecFrom: projectPackage.
						self root: (Smalltalk at: spec className asSymbol) project.
						MetacelloProjectRegistration
							registrationForProjectSpec: spec
							ifPresent: [ :existing |
									existing copyOnWrite: [ :existingCopy |
											spec copyForRegistration: existingCopy onWrite: [ :specCopy |
													specCopy
														ifNil: [ existingCopy merge: spec asProjectRegistration ]
														ifNotNil: [ specCopy mergeScriptRepository: spec ] ] ] ]
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
MetacelloScriptEngine >> handleProjectSpecLoaded: aProjectSpec [

	MetacelloProjectRegistration
		registrationForProjectSpec: aProjectSpec
		ifPresent: [ :existing | "unconditionally merge new with existing (updates registration)"
				existing copyOnWrite: [ :existingCopy |
						existingCopy
							loadedInImage: true;
							merge: aProjectSpec asProjectRegistration ] ]
		ifAbsent: [
				aProjectSpec asProjectRegistration
					loadedInImage: true;
					registerProject ]
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
  self validateProjectSpec.
  self root: self projectReferenceSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> load: required [

	self
		do: [ :versionSpec :requiredArray :projectSpec |
				| result |
				result := versionSpec doLoadRequiredFromArray: requiredArray withEngine: self.
				self root: result.
				MetacelloProjectRegistration
					registrationForProjectSpec: projectSpec
					ifPresent: [ :existing1 |
							existing1 copyOnWrite: [ :existingCopy |
									existingCopy
										loadedInImage: true;
										merge: projectSpec asProjectRegistration ] ]
					ifAbsent: [
							projectSpec asProjectRegistration
								loadedInImage: true;
								registerProject ] ]
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
	MetacelloProjectRegistration
		copyRegistryDuring: [
				self validateProjectSpec.
				spec := self projectReferenceSpec.
				MetacelloProjectRegistration
					registrationForProjectSpec: spec
					ifPresent: [ :existing |
							existing copyOnWrite: [ :existingCopy |
									existingCopy locked: true.
									spec copyForRegistration: existingCopy onWrite: [ :specCopy |
											specCopy
												ifNil: [ existingCopy merge: spec asProjectRegistration ]
												ifNotNil: [
														specCopy mergeScriptRepository: spec.
														spec := specCopy ] ] ] ]
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

	^ MetacelloProjectRegistration
		  registrationForProjectSpec: aProjectSpec
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
MetacelloScriptEngine >> projectReferenceSpec [
    ^ projectReferenceSpec projectReference
]

{ #category : 'accessing' }
MetacelloScriptEngine >> projectReferenceSpec: aProjectSpec [

    projectReferenceSpec := aProjectSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> record: required [

	^ self
		  do: [ :versionSpec :requiredArray :projectSpec | versionSpec doRecordRequiredFromArray: requiredArray withEngine: self ]
		  required: required
		  commitIfSuccess: false
]

{ #category : 'actions api' }
MetacelloScriptEngine >> register [

	| spec |
	self validateProjectSpec.
	spec := self projectReferenceSpec.
	MetacelloProjectRegistration
		registrationForProjectSpec: spec
		ifPresent: [ :existing | existing copyOnWrite: [ :existingCopy | existingCopy merge: spec asProjectRegistration ] ]
		ifAbsent: [ spec asProjectRegistration registerProject ].
	self root: spec
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

{ #category : 'actions api' }
MetacelloScriptEngine >> unlock [

	| spec |
	MetacelloProjectRegistration
		copyRegistryDuring: [
				self validateProjectSpec.
				spec := self projectReferenceSpec.
				MetacelloProjectRegistration
					registrationForProjectSpec: spec
					ifPresent: [ :existing | existing copyOnWrite: [ :existingCopy | existingCopy locked: false ] ].
				self root: spec ]
		commitIfSuccess: true
]

{ #category : 'actions api' }
MetacelloScriptEngine >> unregister [

	| spec |
	self validateProjectSpec.
	spec := self projectReferenceSpec.
	MetacelloProjectRegistration registrationForProjectSpec: spec ifPresent: [ :existing | existing unregisterProject ].
	self root: spec
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> validateProjectSpec [

	self projectReferenceSpec lookForIssues
]
