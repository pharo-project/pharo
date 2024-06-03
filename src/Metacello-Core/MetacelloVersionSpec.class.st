Class {
	#name : 'MetacelloVersionSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'repositories',
		'packages',
		'versionString',
		'packageList',
		'author',
		'timestamp',
		'importArray',
		'postLoadDoIt',
		'blessing',
		'importName',
		'preLoadDoIt',
		'description'
	],
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'visiting' }
MetacelloVersionSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitVersionSpec: self
]

{ #category : 'accessing' }
MetacelloVersionSpec >> author [

	^ author ifNil: [
		  self project valueHolderSpec
			  value: '';
			  yourself ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> author: anObject [

	author := anObject asMetacelloValueHolderFor: self
]

{ #category : 'accessing' }
MetacelloVersionSpec >> blessing [

	^ blessing ifNil: [
		  self project valueHolderSpec
			  value: self project defaultBlessing;
			  yourself ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> blessing: anObject [

	blessing := anObject asMetacelloValueHolderFor: self
]

{ #category : 'printing' }
MetacelloVersionSpec >> configMethodBasicOn: aStream last: last indent: indent [

	| values lastIndex lastBlock |
	last
		ifTrue: [ "need to calculate last statement with a value"
			values := {
				          self getBlessing.
				          self getDescription.
				          self getPreLoadDoIt.
				          self getPostLoadDoIt.
				          self getAuthor.
				          self getTimestamp }.
			1 to: values size do: [ :index | (values at: index) ifNotNil: [ lastIndex := index ] ].
			lastBlock := [ :arg | arg = lastIndex ] ]
		ifFalse: [ lastBlock := [ :arg | false ] ].
	self
		configMethodValueOn: aStream
		for: self getBlessing
		selector: 'blessing:'
		last: (lastBlock value: 1)
		indent: indent.
	self
		configMethodValueOn: aStream
		for: self getDescription
		selector: 'description:'
		last: (lastBlock value: 2)
		indent: indent.
	self
		configMethodValueOn: aStream
		for: self getPreLoadDoIt
		selector: 'preLoadDoIt:'
		last: (lastBlock value: 3)
		indent: indent.
	self
		configMethodValueOn: aStream
		for: self getPostLoadDoIt
		selector: 'postLoadDoIt:'
		last: (lastBlock value: 4)
		indent: indent.
	self
		configMethodValueOn: aStream
		for: self getAuthor
		selector: 'author:'
		last: (lastBlock value: 5)
		indent: indent.
	self
		configMethodValueOn: aStream
		for: self getTimestamp
		selector: 'timestamp:'
		last: (lastBlock value: 6)
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

	self configMethodOn: aStream last: true indent: indent
]

{ #category : 'printing' }
MetacelloVersionSpec >> configMethodOn: aStream last: last indent: indent [

	| spec hasRepositories hasPackageSpecs hasImport |
	hasRepositories := (spec := self repositoriesSpec) isNotNil and: [ spec list isEmpty not ].
	hasImport := self import isNotNil.
	hasPackageSpecs := false.
	self packagesSpec list do: [ :member |
		member spec
			projectDo: [ :proj | member spec name ifNotNil: [ hasPackageSpecs := true ] ]
			packageDo: [ :package | member spec name ifNotNil: [ hasPackageSpecs := true ] ]
			groupDo: [ :group | member spec name ifNotNil: [ hasPackageSpecs := true ] ] ].
	self configMethodBasicOn: aStream last: (hasRepositories | hasPackageSpecs | hasImport) not indent: indent.
	hasImport ifTrue: [ self configMethodValueOn: aStream for: self import selector: 'import:' last: (hasRepositories | hasPackageSpecs) not indent: indent ].
	hasRepositories ifTrue: [
		spec map values size = 1
			ifTrue: [
				aStream
					tab: indent;
					nextPutAll: 'spec repository: ';
					nextPutAll: spec map values first description printString;
					nextPutAll: '.'.
				hasPackageSpecs ifTrue: [ aStream cr ] ]
			ifFalse: [ self configMethodOn: aStream for: spec selector: 'repositories:' last: hasPackageSpecs not indent: indent ] ].
	self configPackagesSpecMethodOn: aStream indent: indent.
	last ifFalse: [ aStream cr ]
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
	projectSpecs isEmpty not ifTrue: [
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
	packageSpecs isEmpty not ifTrue: [
		projectSpecs isEmpty not ifTrue: [ aStream cr ].
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
	groupSpecs isEmpty not ifTrue: [
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

{ #category : 'private' }
MetacelloVersionSpec >> createVersion [

	^ self versionClass fromSpec: self
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
				 do: [ :ex | ex return: nil ]) ifNotNil: [ :workingCopy |
				workingCopy systemPackage ifNotNil: [ :package |
					classes addAll: package classes ] ] ]
		groupDo: [ :ignored |  ].
	^ classes
]

{ #category : 'loading' }
MetacelloVersionSpec >> defaultPackageNames [
	"if there is a package named 'default' (a group) then it defines the default package names,
	 otherwise answer a list of all of the package names in this version"

	self packages packageNamed: 'default' ifAbsent: [ ^self packageNames ].
	^#('default')
]

{ #category : 'toolbox support' }
MetacelloVersionSpec >> deleteSpec: aSpec [
    "remove the spec from packages"

    self packages deleteSpec: aSpec
]

{ #category : 'accessing' }
MetacelloVersionSpec >> description [

	^ description ifNil: [
		  self project valueHolderSpec
			  value: '';
			  yourself ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> description: anObject [

	description := anObject asMetacelloValueHolderFor: self
]

{ #category : 'querying' }
MetacelloVersionSpec >> difference: otherVersionSpec [
	"Return a dictionary of additions, removals and modifications"

	| report myProjectSpecs myPackageSpecs otherProjectSpecs otherPackageSpecs |
	report := MetacelloVersionDiffReport new.
	myProjectSpecs := Dictionary new.
	myPackageSpecs := Dictionary new.
	self
		projectDo: [ :projectSpec | myProjectSpecs at: projectSpec name put: projectSpec ]
		packageDo: [ :packageSpec | myPackageSpecs at: packageSpec name put: packageSpec ]
		groupDo: [ :ignored |  ].
	otherProjectSpecs := Dictionary new.
	otherPackageSpecs := Dictionary new.
	otherVersionSpec
		projectDo: [ :projectSpec | otherProjectSpecs at: projectSpec name put: projectSpec ]
		packageDo: [ :packageSpec | otherPackageSpecs at: packageSpec name put: packageSpec ]
		groupDo: [ :ignored |  ].
	myProjectSpecs valuesDo: [ :myProjectSpec |
		(otherProjectSpecs at: myProjectSpec name ifAbsent: [  ])
			ifNotNil: [ :otherProjectSpec |
				myProjectSpec versionString = otherProjectSpec versionString ifFalse: [
					report modifications at: myProjectSpec name put: {
							myProjectSpec versionString.
							otherProjectSpec versionString } ] ]
			ifNil: [
				report removals at: myProjectSpec name put: {
						myProjectSpec versionString.
						'' } ] ].
	otherProjectSpecs valuesDo: [ :otherProjectSpec |
		(myProjectSpecs at: otherProjectSpec name ifAbsent: [  ]) ifNil: [
			report additions at: otherProjectSpec name put: {
					''.
					otherProjectSpec versionString } ] ].
	myPackageSpecs valuesDo: [ :myPackageSpec |
		| otherPackageSpec |
		otherPackageSpec := otherPackageSpecs at: myPackageSpec name ifAbsent: [  ].
		otherPackageSpec
			ifNotNil: [
				myPackageSpec file = otherPackageSpec file ifFalse: [
					report modifications at: myPackageSpec name put: {
							myPackageSpec file.
							otherPackageSpec file } ] ]
			ifNil: [
				report removals at: myPackageSpec name put: {
						myPackageSpec file.
						'' } ] ].
	otherPackageSpecs valuesDo: [ :otherPackageSpec |
		(myPackageSpecs at: otherPackageSpec name ifAbsent: [  ]) ifNil: [
			report additions at: otherPackageSpec name put: {
					''.
					otherPackageSpec file } ] ].
	^ report
]

{ #category : 'accessing' }
MetacelloVersionSpec >> getAuthor [
	^author
]

{ #category : 'accessing' }
MetacelloVersionSpec >> getBlessing [
	^blessing
]

{ #category : 'accessing' }
MetacelloVersionSpec >> getDescription [
	^description
]

{ #category : 'accessing' }
MetacelloVersionSpec >> getPostLoadDoIt [
	^postLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> getPreLoadDoIt [
	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> getTimestamp [
	^timestamp
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

	^self versionString, ' [', self projectLabel, ']'
]

{ #category : 'accessing' }
MetacelloVersionSpec >> loader: aMetacelloLoadTarget [ 
	
	"nothing"
]

{ #category : 'merging' }
MetacelloVersionSpec >> mergeMap [
    | map |
    map := super mergeMap.
    map at: #'versionString' put: versionString.
    map at: #'blessing' put: blessing.
    map at: #'description' put: description.
    map at: #'author' put: author.
    map at: #'timestamp' put: timestamp.
    map at: #'preLoadDoIt' put: preLoadDoIt.
    map at: #'postLoadDoIt' put: postLoadDoIt.
    map at: #'packageList' put: self packages.
    map at: #'repositories' put: self repositories.
    ^ map
]

{ #category : 'merging' }
MetacelloVersionSpec >> mergeSpec: anotherSpec [

	| newSpec map anotherPackages anotherRepositories |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(anotherPackages := map at: #packageList) isEmpty not ifTrue: [
		newSpec packages: (self packages isEmpty
				 ifTrue: [ anotherPackages ]
				 ifFalse: [ self packages mergeSpec: anotherPackages ]) ].
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

	^self packageNamed: aString ifAbsent: [ nil ]
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

	^self packageNamed: aString forMap: self packages map ifAbsent: aBlock
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNames [
	"leave reference to packages for upgrade purposes"

	packages ifNil: [ packageList ifNil: [ ^ #(  ) ] ].
	^ self packages map keys asSet
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageSpecsInLoadOrder [

	^ self packages packageSpecsInLoadOrderFor: self
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

{ #category : 'querying' }
MetacelloVersionSpec >> packageSpecsInLoadOrderWithEngine: anObject [
  ^ self packages packageSpecsInLoadOrderFor: self
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packages [
	"leave reference to packages for upgrade purposes"

	^ packages ifNil: [ packageList ifNil: [ packageList := self project packagesSpec ] ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packages: anObject [
    packageList := anObject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packagesSpec [

	^self packages
]

{ #category : 'copying' }
MetacelloVersionSpec >> postCopy [
    super postCopy.
  blessing := blessing copy.
    description := description copy.
    author := author copy.
    timestamp := timestamp copy.
    packageList := packageList copy.
    repositories := repositories copy.
    packages := packages copy	"leave reference to packages for upgrade purposes"
]

{ #category : 'querying' }
MetacelloVersionSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> postLoadDoIt: anObject [

	anObject setPostLoadDoItInMetacelloSpec: self
]

{ #category : 'querying' }
MetacelloVersionSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> preLoadDoIt: anObject [

	anObject setPreLoadDoItInMetacelloSpec: self
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	self packageSpecsInLoadOrder do: [ :pkgSpec |
		pkgSpec
			projectDo: projectBlock
			packageDo: packageBlock
			groupDo: groupBlock ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> projectLabel [

	^self project label
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories [

	^ repositories ifNil: [ repositories := self project repositoriesSpec ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories: anObject [
	repositories := anObject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositoriesSpec [

	^self repositories
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString [
	self repositoriesSpec add: aString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString username: username password: password [
	self repositoriesSpec repository: aString username: username password: password
]

{ #category : 'loading' }
MetacelloVersionSpec >> repositorySpecs [

	^self repositories map values
]

{ #category : 'loading' }
MetacelloVersionSpec >> resolveToLoadableSpec: aString forMap: map packages: packageMap [

	| package |
	package := self packageNamed: aString forMap: map ifAbsent: [ ^ self error: 'Name not found: ' , aString ].
	packageMap at: package name put: package.
	^ { package }
]

{ #category : 'loading' }
MetacelloVersionSpec >> resolveToLoadableSpecs: nameList [
	"Resolves names in namelist to the  list of packages and projects in the version that would be loaded. Projects are 
	not traversed during the transitive closure. The scope is that of the version itself.
	   If the spec is a package, answer a list including the package and the transitive closure on 
			its #requires: and #includes: fields.
	   If the spec is a project, answer the project.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"

	| map |
	map := Dictionary new.
	self resolveToLoadableSpecs: nameList map: map.
	^ map values
]

{ #category : 'loading' }
MetacelloVersionSpec >> resolveToLoadableSpecs: required map: packageMap [

	| reqd allReqd map newReqd |
	reqd := required copy.
	allReqd := Set new.
	map := self packages map.
	[ reqd isEmpty ] whileFalse: [
		newReqd := Set new.
		reqd do: [ :req |
			(self resolveToLoadableSpec: req forMap: map packages: packageMap) do: [ :loadableSpec |
				newReqd addAll: loadableSpec requires.
				newReqd addAll: loadableSpec includes ] ].
		allReqd addAll: reqd.
		newReqd removeAllFoundIn: allReqd.
		reqd := newReqd ].
	packageMap keys do: [ :pkgName |
		(packageMap at: pkgName) resolveToLoadableSpec
			ifNotNil: [ packageMap at: pkgName put: (packageMap at: pkgName) resolveToLoadableSpec ]
			ifNil: [ packageMap removeKey: pkgName ] ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> setImport: anArray [
  importArray := importArray
    ifNil: [ anArray ]
    ifNotNil: [ importArray , anArray ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> setPostLoadDoIt: aSymbol [

	postLoadDoIt := aSymbol
]

{ #category : 'accessing' }
MetacelloVersionSpec >> setPreLoadDoIt: aSymbol [

	preLoadDoIt := aSymbol
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> specListProjectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	self packages specListDo: [:pkgSpec |
		pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> specsNamed: packageAndProjectNames projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	| map |
	map := self packages map.
	packageAndProjectNames do: [ :name |
		(map at: name ifAbsent: [  ]) ifNotNil: [ :pkgSpec | pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ] ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> timestamp [

	^ timestamp ifNil: [
		  self project valueHolderSpec
			  value: '';
			  yourself ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> timestamp: anObject [

	timestamp := anObject asMetacelloValueHolderFor: self
]

{ #category : 'private' }
MetacelloVersionSpec >> versionClass [

	^ MetacelloVersion
]

{ #category : 'private' }
MetacelloVersionSpec >> versionNumber [
    ^ self project versionNumberClass fromString: self versionString
]

{ #category : 'querying' }
MetacelloVersionSpec >> versionString [

	^ versionString ifNil: [ '' ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> versionString: anObject [
	versionString := anObject
]
