Class {
	#name : 'MetacelloVersionSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'project',
		'repositories',
		'importArray',
		'postLoadDoIt',
		'importName',
		'preLoadDoIt',
		'packagesSpec'
	],
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'class initialization' }
MetacelloVersionSpec class >> initialize [
	"Added in Pharo 14."

	self deprecatedAliases: #( MetacelloVersion )
]

{ #category : 'instance creation' }
MetacelloVersionSpec class >> project: aMetacelloProject [

	^ self new
		  project: aMetacelloProject;
		  yourself
]

{ #category : 'visiting' }
MetacelloVersionSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitVersionSpec: self
]

{ #category : 'printing' }
MetacelloVersionSpec >> configMethodBasicOn: aStream last: last indent: indent [

	| values lastIndex lastBlock |
	last
		ifTrue: [ "need to calculate last statement with a value"
				values := {
					          self preLoadDoIt.
					          self postLoadDoIt }.
				1 to: values size do: [ :index | (values at: index) ifNotNil: [ lastIndex := index ] ].
				lastBlock := [ :arg | arg = lastIndex ] ]
		ifFalse: [ lastBlock := [ :arg | false ] ].
	self
		configMethodValueOn: aStream
		for: self preLoadDoIt
		selector: 'preLoadDoIt:'
		last: (lastBlock value: 1)
		indent: indent.
	self
		configMethodValueOn: aStream
		for: self postLoadDoIt
		selector: 'postLoadDoIt:'
		last: (lastBlock value: 2)
		indent: indent
]

{ #category : 'printing' }
MetacelloVersionSpec >> configMethodOn: aStream for: spec selector: selector last: last indent: indent [

	spec ifNil: [ ^ self ].
	aStream
		tab: indent;
		nextPutAll: 'spec ' , selector , ' [';
		cr.
	spec configMethodOn: aStream indent: indent + 1.
	aStream nextPutAll: ' ].'.
	last ifFalse: [ aStream cr ]
]

{ #category : 'printing' }
MetacelloVersionSpec >> configMethodOn: aStream indent: indent [

	| spec hasRepositories hasPackageSpecs hasImport |
	hasRepositories := (spec := self repositories) isNotNil and: [ spec list isEmpty not ].
	hasImport := self import isNotNil.
	hasPackageSpecs := false.
	self packagesSpec list do: [ :member |
			member spec
				projectDo: [ :proj | member spec name ifNotNil: [ hasPackageSpecs := true ] ]
				packageDo: [ :package | member spec name ifNotNil: [ hasPackageSpecs := true ] ]
				groupDo: [ :group | member spec name ifNotNil: [ hasPackageSpecs := true ] ] ].
	self configMethodBasicOn: aStream last: (hasRepositories | hasPackageSpecs | hasImport) not indent: indent.
	hasImport ifTrue: [
			self
				configMethodValueOn: aStream
				for: self import
				selector: 'import:'
				last: (hasRepositories | hasPackageSpecs) not
				indent: indent ].
	hasRepositories ifTrue: [
			spec map values size = 1
				ifTrue: [
						aStream
							tab: indent;
							nextPutAll: 'spec repository: ';
							nextPutAll: spec map values first description printString;
							nextPutAll: '.'.
						hasPackageSpecs ifTrue: [ aStream cr ] ]
				ifFalse: [
						self
							configMethodOn: aStream
							for: spec
							selector: 'repositories:'
							last: hasPackageSpecs not
							indent: indent ] ].
	self configPackagesSpecMethodOn: aStream indent: indent
]

{ #category : 'printing' }
MetacelloVersionSpec >> configMethodValueOn: aStream for: spec selector: selector last: last indent: indent [

	| valuePrintString |
	spec ifNil: [ ^ self ].
	valuePrintString := spec value isSymbol
		                    ifTrue: [ '#' , spec value asString printString ]
		                    ifFalse: [ spec value printString ].
	aStream
		tab: indent;
		nextPutAll: 'spec ' , selector , ' ' , valuePrintString , '.'.
	last ifFalse: [ aStream cr ]
]

{ #category : 'printing' }
MetacelloVersionSpec >> configPackagesSpecMethodOn: aStream indent: indent [

	| projectSpecs packageSpecs groupSpecs |
	projectSpecs := OrderedCollection new.
	packageSpecs := OrderedCollection new.
	groupSpecs := OrderedCollection new.
	self packagesSpec list do: [ :member |
			member spec
				projectDo: [ :proj | member spec name ifNotNil: [ projectSpecs add: member ] ]
				packageDo: [ :package | member spec name ifNotNil: [ packageSpecs add: member ] ]
				groupDo: [ :group | member spec name ifNotNil: [ groupSpecs add: member ] ] ].
	projectSpecs isEmpty ifFalse: [
			aStream
				tab: indent;
				nextPutAll: 'spec '.
			projectSpecs size > 1 ifTrue: [
					aStream
						cr;
						tab: indent + 1 ].
			1 to: projectSpecs size do: [ :index |
					(projectSpecs at: index) configMethodCascadeOn: aStream last: index == projectSpecs size indent: indent + 1.
					index ~= projectSpecs size ifTrue: [ aStream tab: indent + 1 ] ] ].
	packageSpecs isEmpty ifFalse: [
			projectSpecs isEmpty ifFalse: [ aStream cr ].
			aStream
				tab: indent;
				nextPutAll: 'spec '.
			packageSpecs size > 1 ifTrue: [
					aStream
						cr;
						tab: indent + 1 ].
			1 to: packageSpecs size do: [ :index |
					(packageSpecs at: index) configMethodCascadeOn: aStream last: index == packageSpecs size indent: indent + 1.
					index ~= packageSpecs size ifTrue: [ aStream tab: indent + 1 ] ] ].
	groupSpecs isEmpty ifFalse: [
			projectSpecs isEmpty not | packageSpecs isEmpty not ifTrue: [ aStream cr ].
			aStream
				tab: indent;
				nextPutAll: 'spec '.
			groupSpecs size > 1 ifTrue: [
					aStream
						cr;
						tab: indent + 1 ].
			1 to: groupSpecs size do: [ :index |
					(groupSpecs at: index) configMethodCascadeOn: aStream last: index == groupSpecs size indent: indent + 1.
					index ~= groupSpecs size ifTrue: [ aStream tab: indent + 1 ] ] ]
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> currentlyLoadedClassesInVersion [

	| classes |
	classes := Set new.
	self
		projectDo: [ :ignored |  ]
		packageDo: [ :packageSpec |
				([ packageSpec workingCopy ]
					 on: Error
					 do: [ :ex | ex return: nil ]) ifNotNil: [ :workingCopy | workingCopy systemPackage ifNotNil: [ :package | classes addAll: package classes ] ] ]
		groupDo: [ :ignored |  ].
	^ classes
]

{ #category : 'loading' }
MetacelloVersionSpec >> defaultPackageNames [
	"if there is a package named 'default' (a group) then it defines the default package names,
	 otherwise answer a list of all of the package names in this version"

	self packagesSpec packageNamed: 'default' ifAbsent: [ ^ self packageNames ].
	^ #( 'default' )
]

{ #category : 'private' }
MetacelloVersionSpec >> doFetchRequiredFromArray: anArray withEngine: anEngine [

	^ self doWithProgress: [ self fetchRequiredFromArray: anArray withEngine: anEngine ] label: 'Fetching'
]

{ #category : 'private' }
MetacelloVersionSpec >> doLoadRequiredFromArray: anArray withEngine: anEngine [

	^ self doWithProgress: [ self loadRequiredFromArray: anArray withEngine: anEngine ] label: 'Loading'
]

{ #category : 'private' }
MetacelloVersionSpec >> doRecordRequiredFromArray: anArray withEngine: anEngine [

	^ self doWithProgress: [ self recordRequiredFromArray: anArray withEngine: anEngine ] label: 'Recording'
]

{ #category : 'private' }
MetacelloVersionSpec >> doWithProgress: aBlock label: aLabel [

	| displayString result |
	displayString := aLabel , ' baseline of ' , self projectLabel.
	MetacelloNotification signal: displayString , '...'.

	displayString displayProgressFrom: 0 to: 2 during: [ :bar |
			bar value: 1.
			result := aBlock value.
			bar value: 2 ].

	MetacelloNotification signal: '...finished to load the baseline'.
	^ result
]

{ #category : 'private' }
MetacelloVersionSpec >> fetchRequiredFromArray: anArray withEngine: anEngine [

	| directive |
	directive := self recordRequiredFromArray: (anArray ifEmpty: [ self defaultPackageNames ]) withEngine: anEngine.

	directive acceptVisitor: (MetacelloFetchTarget on: anEngine).
	^ directive
]

{ #category : 'accessing' }
MetacelloVersionSpec >> groups [
	"Answers the list of groups associated with this version"

	| groups |
	groups := OrderedCollection new.
	self projectDo: [ :ignored |  ] packageDo: [ :ignored |  ] groupDo: [ :grp | groups add: grp ].
	^ groups
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import [
    ^ importName
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import: aString [
  importName := aString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import: aString provides: anArray [

	importArray := importArray
		               ifNil: [ { (aString -> anArray) } ]
		               ifNotNil: [ importArray , { (aString -> anArray) } ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> importArray [
  ^ importArray
]

{ #category : 'printing' }
MetacelloVersionSpec >> label [

	^ 'baseline [' , self projectLabel , ']'
]

{ #category : 'private' }
MetacelloVersionSpec >> loadRequiredFromArray: anArray withEngine: anEngine [

	| directive |
	directive := self fetchRequiredFromArray: (anArray ifEmpty: [ self defaultPackageNames ]) withEngine: anEngine.

	directive acceptVisitor: (MetacelloLoadTarget on: anEngine).
	^ directive
]

{ #category : 'accessing' }
MetacelloVersionSpec >> loader: aMetacelloLoadTarget [ 
	
	"nothing"
]

{ #category : 'merging' }
MetacelloVersionSpec >> mergeMap [

	^ super mergeMap
		  at: #preLoadDoIt put: preLoadDoIt;
		  at: #postLoadDoIt put: postLoadDoIt;
		  at: #packageList put: self packagesSpec;
		  at: #repositories put: self repositories;
		  yourself
]

{ #category : 'merging' }
MetacelloVersionSpec >> mergeSpec: anotherSpec [

	| newSpec map anotherPackages anotherRepositories |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(anotherPackages := map at: #packageList) isEmpty not ifTrue: [
			newSpec packagesSpec: (self packagesSpec isEmpty
					 ifTrue: [ anotherPackages ]
					 ifFalse: [ self packagesSpec mergeSpec: anotherPackages ]) ].
	(anotherRepositories := map at: #repositories) isEmpty not ifTrue: [
			newSpec repositories: (self repositories isEmpty
					 ifTrue: [ anotherRepositories ]
					 ifFalse: [ self repositories mergeSpec: anotherRepositories ]) ].
	^ newSpec
]

{ #category : 'merging' }
MetacelloVersionSpec >> nonOverridable [
    ^ super nonOverridable , #(#'packageList' #'repositories')
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNamed: aString [

	^ self packageNamed: aString ifAbsent: [ nil ]
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNamed: aString forMap: map ifAbsent: absentBlock [

	| importSpec |
	^ map at: aString ifAbsent: [
		  (self importArray isNotNil or: [ self import isNotNil ]) ifTrue: [ "expect the 'missing' name to be satisfied within context of imported project"
			  importArray ifNotNil: [
				  importArray do: [ :assoc |
					  ((assoc value includes: aString) and: [ map includesKey: assoc key ]) ifTrue: [
						  importSpec := (map at: assoc key)
							                mergeImportLoads: { aString };
							                yourself ] ].
				  importSpec ifNotNil: [ ^ importSpec ] ].
			  (importSpec isNil and: [ self import isNotNil ]) ifTrue: [
				  ^ (map at: self import ifAbsent: absentBlock)
					    mergeImportLoads: { aString };
					    yourself ] ].
		  (aString = 'default' or: [ aString = 'ALL' ])
			  ifTrue: [
				  self project groupSpec
					  name: aString;
					  includes: self packageNames;
					  yourself ]
			  ifFalse: [ absentBlock value ] ]
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNamed: aString ifAbsent: aBlock [

	^ self packageNamed: aString forMap: self packagesSpec map ifAbsent: aBlock
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNames [

	^ self packagesSpec map keys asSet
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageSpecsInLoadOrder [

	^ self packagesSpec packageSpecsInLoadOrderFor: self
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageSpecsInLoadOrderForMap: packageMap [

	| loadOrder pkgs packageNames importNames importSpec importProjectSpecs importProjectNameMap |
	loadOrder := self packageSpecsInLoadOrder.
	importNames := (packageNames := (packageMap values collect: [ :pkg |
		                                 pkg name ]) asSet) copy.
	(self import isNil and: [ self importArray isNil ]) ifTrue: [
		^ loadOrder select: [ :pkg | packageNames includes: pkg name ] ].
	loadOrder do: [ :pkg | importNames remove: pkg name ifAbsent: [  ] ].
	pkgs := OrderedCollection new.
	importProjectSpecs := Dictionary new.
	importProjectNameMap := Dictionary new.
	importArray ifNotNil: [
		loadOrder do: [ :pkg |
			importArray do: [ :assoc |
				assoc key = pkg name ifTrue: [
					importProjectSpecs at: pkg name put: pkg.
					(assoc value select: [ :each | importNames includes: each ]) do: [
						:each |
						(importProjectNameMap
							 at: pkg name
							 ifAbsent: [ importProjectNameMap at: pkg name put: Set new ])
							add: each ] ] ] ] ].
	self import ifNotNil: [
		loadOrder do: [ :pkg |
			pkg name = self import ifTrue: [
				importProjectSpecs at: pkg name put: pkg.
				importProjectNameMap at: pkg name put: importNames ] ] ].
	loadOrder do: [ :pkg |
		(packageNames includes: pkg name) ifTrue: [ pkgs add: pkg ].
		importProjectSpecs
			at: pkg name
			ifPresent: [ :importProjectSpec | "insert the imports at this point"
				(importProjectNameMap at: pkg name ifAbsent: [ #(  ) ]) do: [
					:importedName |
					pkgs add: (importSpec := importProjectSpec copy
							               name: importedName;
							               mergeImportLoads: { importedName };
							               yourself).
					importSpec projectReference name: importedName ] ] ].
	^ pkgs
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packages [
	"Answers the list of packages associated with this version"

	| packages |
	packages := OrderedCollection new.
	self projectDo: [ :ignored |  ] packageDo: [ :pkg | packages add: pkg ] groupDo: [ :ignored |  ].
	^ packages
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packagesSpec [

	^ packagesSpec ifNil: [ packagesSpec := MetacelloPackagesSpec new ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packagesSpec: anObject [

	packagesSpec := anObject
]

{ #category : 'copying' }
MetacelloVersionSpec >> postCopy [

	super postCopy.
	packagesSpec := packagesSpec copy.
	repositories := repositories copy
]

{ #category : 'querying' }
MetacelloVersionSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> postLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	postLoadDoIt := aSymbol
]

{ #category : 'querying' }
MetacelloVersionSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> preLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	preLoadDoIt := aSymbol
]

{ #category : 'accessing' }
MetacelloVersionSpec >> project [

	^ project
]

{ #category : 'accessing' }
MetacelloVersionSpec >> project: aMetacelloProject [

	project := aMetacelloProject
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	self packageSpecsInLoadOrder do: [ :pkgSpec | pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> projectLabel [

	^self project label
]

{ #category : 'accessing' }
MetacelloVersionSpec >> projects [
	"Answers the list of projects associated with this version"

	| projects |
	projects := OrderedCollection new.
	self projectDo: [ :prj | projects add: prj projectReference ] packageDo: [ :ignored |  ] groupDo: [ :ignored |  ].
	^ projects
]

{ #category : 'private' }
MetacelloVersionSpec >> recordRequiredFromArray: anArray withEngine: anEngine [

	| target |
	target := MetacelloRecordTarget on: anEngine.
	target required: anArray.

	self acceptVisitor: target.

	^ target specBuilder root
]

{ #category : 'actions' }
MetacelloVersionSpec >> recordWithEngine: anEngine [

	^ self doRecordRequiredFromArray: self defaultPackageNames withEngine: anEngine
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories [

	^ repositories ifNil: [ repositories := MetacelloRepositoriesSpec new ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories: anObject [
	repositories := anObject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString [

	self repositories add: aString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString username: username password: password [

	self repositories repository: aString username: username password: password
]

{ #category : 'loading' }
MetacelloVersionSpec >> repositorySpecs [

	^self repositories map values
]

{ #category : 'loading' }
MetacelloVersionSpec >> resolveToLoadableSpecs: aRequiredCollection [

	| packageMap required managedRequirements map |
	packageMap := Dictionary new.
	required := aRequiredCollection copy.
	managedRequirements := Set new.
	map := self packagesSpec map.
	[ required isEmpty ] whileFalse: [
			| newReqd |
			newReqd := Set new.
			required do: [ :req |
					| loadableSpec |
					loadableSpec := self packageNamed: req forMap: map ifAbsent: [ ^ self error: 'Name not found: ' , req ]. "If the loadable spec is a Group we ignore it."
					loadableSpec resolveToLoadableSpec ifNotNil: [ :resolvedSpec | packageMap at: loadableSpec name put: resolvedSpec ].
					newReqd addAll: loadableSpec requires.
					newReqd addAll: loadableSpec includes ].
			managedRequirements addAll: required.
			newReqd removeAllFoundIn: managedRequirements.
			required := newReqd ].
	^ packageMap
]

{ #category : 'accessing' }
MetacelloVersionSpec >> setImport: anArray [
  importArray := importArray
    ifNil: [ anArray ]
    ifNotNil: [ importArray , anArray ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> spec [
	"To remove once Iceberg is fixed."

	^ self
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> specListProjectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	self packagesSpec specListDo: [ :pkgSpec | pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]
]
