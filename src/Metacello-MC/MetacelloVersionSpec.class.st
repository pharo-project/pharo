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
	#category : 'Metacello-MC-Specs',
	#package : 'Metacello-MC',
	#tag : 'Specs'
}

{ #category : 'class initialization' }
MetacelloVersionSpec class >> initialize [

	self deprecatedAliases: { #MetacelloMCVersionSpec }
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

	anObject setAuthorInMetacelloVersion: self
]

{ #category : 'construction' }
MetacelloVersionSpec >> author: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor authorForVersion: aBlockOrString
]

{ #category : 'construction' }
MetacelloVersionSpec >> baseline: aString constructor: aVersionConstructor [
    aVersionConstructor baselineForVersion: aString
]

{ #category : 'construction' }
MetacelloVersionSpec >> baseline: aString with: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor baselineForVersion: aString with: aBlockOrString
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

	anObject setBlessingInMetacelloVersion: self
]

{ #category : 'construction' }
MetacelloVersionSpec >> blessing: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor blessingForVersion: aBlockOrString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> computeVersionStatus: matchBlock [
	self computeVersionStatus: (self expandToLoadableSpecNames: #('ALL')) matchBlock: matchBlock
]

{ #category : 'accessing' }
MetacelloVersionSpec >> computeVersionStatus: resolvedPackageAndProjectNames matchBlock: matchBlock [

	| status |

	status := resolvedPackageAndProjectNames
		ifNil: [ self isPartiallyCurrent ]
		ifNotNil: [ self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames ].
	status isAllLoadedToSpec: matchBlock.
	status isLoadedToSpec: matchBlock.
	status isLoadedMatchConstraints: matchBlock.
	status isSomethingLoaded: matchBlock
]

{ #category : 'printing' }
MetacelloVersionSpec >> configMethodBasicOn: aStream last: last indent: indent [
    | values lastIndex lastBlock |
    last
        ifTrue: [ 
            "need to calculate last statement with a value"
            values := {(self getBlessing).
            (self getDescription).
            (self getPreLoadDoIt).
            (self getPostLoadDoIt).
            (self getAuthor).
            (self getTimestamp)}.
            1 to: values size do: [ :index | 
                (values at: index) ~~ nil
                    ifTrue: [ lastIndex := index ] ].
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
	self packagesSpec list do: [:member |
		member spec 
			projectDo: [:proj | member spec name ~~ nil ifTrue: [ projectSpecs add: member ]]
			packageDo: [:package | member spec name ~~ nil ifTrue: [ packageSpecs add: member ]]
			groupDo: [:group | member spec name ~~ nil ifTrue: [ groupSpecs add: member ]]].
	projectSpecs isEmpty not
		ifTrue: [
			aStream tab: indent; nextPutAll: 'spec '.
			projectSpecs size > 1 ifTrue: [ aStream cr; tab: indent + 1 ].
			1 to: projectSpecs size  do: [:index | 
				(projectSpecs at: index) configMethodCascadeOn: aStream last: index == projectSpecs size indent: indent + 1.
				index ~= projectSpecs size ifTrue: [ aStream tab: indent + 1 ]]].
	packageSpecs isEmpty not
		ifTrue: [
			projectSpecs isEmpty not ifTrue: [ aStream cr ].
			aStream tab: indent; nextPutAll: 'spec '.
			packageSpecs size > 1 ifTrue: [ aStream cr; tab: indent + 1 ].
			1 to: packageSpecs size  do: [:index | 
				(packageSpecs at: index) configMethodCascadeOn: aStream last: index == packageSpecs size indent: indent + 1.
				index ~= packageSpecs size ifTrue: [ aStream tab: indent + 1 ]]].
	groupSpecs isEmpty not
		ifTrue: [
			projectSpecs isEmpty not | packageSpecs isEmpty not ifTrue: [ aStream cr ].
			aStream tab: indent; nextPutAll: 'spec '.
			groupSpecs size > 1 ifTrue: [ aStream cr; tab: indent + 1 ].
			1 to: groupSpecs size  do: [:index | 
				(groupSpecs at: index) configMethodCascadeOn: aStream last: index == groupSpecs size indent: indent + 1.
				index ~= groupSpecs size ifTrue: [ aStream tab: indent + 1 ]]].
]

{ #category : 'construction' }
MetacelloVersionSpec >> configuration: aString with: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor configurationForVersion: aString with: aBlockOrString
]

{ #category : 'private' }
MetacelloVersionSpec >> createVersion [

	^self versionClass fromSpec: self
]

{ #category : 'querying' }
MetacelloVersionSpec >> currentlyLoadedClassesInVersion [
  | classes |
  classes := Set new.
  self
    projectDo: [ :ignored |  ]
    packageDo: [ :packageSpec | 
      | wc |
      wc := [ packageSpec workingCopy ]
        on: Error
        do: [ :ex | ex return: nil ].
      wc ~~ nil
        ifTrue: [ classes addAll: wc packageSet classes ] ]
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

	anObject setDescriptionInMetacelloVersion: self
]

{ #category : 'construction' }
MetacelloVersionSpec >> description: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor descriptionForVersion: aBlockOrString
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
		| otherProjectSpec |
		otherProjectSpec := otherProjectSpecs at: myProjectSpec name ifAbsent: [  ].
		otherProjectSpec == nil
			ifTrue: [
				report removals at: myProjectSpec name put: {
						myProjectSpec versionString.
						'' } ]
			ifFalse: [
				myProjectSpec versionString = otherProjectSpec versionString ifFalse: [
					report modifications at: myProjectSpec name put: {
							myProjectSpec versionString.
							otherProjectSpec versionString } ] ] ].
	otherProjectSpecs valuesDo: [ :otherProjectSpec |
		(myProjectSpecs at: otherProjectSpec name ifAbsent: [  ]) ifNil: [
			report additions at: otherProjectSpec name put: {
					''.
					otherProjectSpec versionString } ] ].
	myPackageSpecs valuesDo: [ :myPackageSpec |
		| otherPackageSpec |
		otherPackageSpec := otherPackageSpecs at: myPackageSpec name ifAbsent: [  ].
		otherPackageSpec == nil
			ifTrue: [
				report removals at: myPackageSpec name put: {
						myPackageSpec file.
						'' } ]
			ifFalse: [
				myPackageSpec file = otherPackageSpec file ifFalse: [
					report modifications at: myPackageSpec name put: {
							myPackageSpec file.
							otherPackageSpec file } ] ] ].
	otherPackageSpecs valuesDo: [ :otherPackageSpec |
		(myPackageSpecs at: otherPackageSpec name ifAbsent: [  ]) ifNil: [
			report additions at: otherPackageSpec name put: {
					''.
					otherPackageSpec file } ] ].
	^ report
]

{ #category : 'loading' }
MetacelloVersionSpec >> expandToLoadableSpecNames: nameList [

	| cacheKey names |
	cacheKey := Array with: self label with: nameList.
	^MetacelloPlatform current
		stackCacheFor: #loadableSpecNames
		at: cacheKey
		doing: [ :cache | 
			names := (self resolveToLoadableSpecs: nameList) collect: [:spec | spec name ].
			cache at: cacheKey put: names ].
]

{ #category : 'development support' }
MetacelloVersionSpec >> forceUpdatedPackageSpecs [

	| updatedSpecs mcLoader |
	updatedSpecs := Dictionary new.
	mcLoader := self loader.
	self packages map valuesDo: [:pkg | pkg forceUpdatePackageSpec: updatedSpecs using: mcLoader].
	^updatedSpecs
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

{ #category : 'construction' }
MetacelloVersionSpec >> group: aString overrides: aStringOrCollection constructor: aVersionConstructor [
    aVersionConstructor groupForVersion: aString overrides: aStringOrCollection
]

{ #category : 'construction' }
MetacelloVersionSpec >> group: aString with: aStringOrCollection constructor: aVersionConstructor [
    aVersionConstructor groupForVersion: aString with: aStringOrCollection
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import [
    ^ importName
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import: aString [
  importName := aString
]

{ #category : 'construction' }
MetacelloVersionSpec >> import: aString constructor: aVersionConstructor [
    aVersionConstructor importForVersion: aString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import: aString provides: anArray [
  importArray := importArray
    ifNil: [ {(aString -> anArray)} ]
    ifNotNil: [ importArray , {(aString -> anArray)} ]
]

{ #category : 'construction' }
MetacelloVersionSpec >> import: aString provides: anArray constructor: aVersionConstructor [
  aVersionConstructor importForVersion: aString provides: anArray
]

{ #category : 'accessing' }
MetacelloVersionSpec >> importArray [
  ^ importArray
]

{ #category : 'testing' }
MetacelloVersionSpec >> isAllLoadedToSpec [
	"all projects and packages are loaded and match specification"

	self isPartiallyCurrent isAllLoadedToSpec: [ :ignored | ^ true ].
	^ false
]

{ #category : 'testing' }
MetacelloVersionSpec >> isAllLoadedToSpecAgainst: resolvedPackageAndProjectNames [
	"all projects and packages are loaded and match specification"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isAllLoadedToSpec: [ :ignored | ^ true ].
	^ false
]

{ #category : 'testing' }
MetacelloVersionSpec >> isLoadedMatchConstraints [
	"all loaded projects and packages match constraints (at least one package loaded)"
	
	self isPartiallyCurrent isLoadedMatchConstraints: [ :ignored | ^ true ].
	^ false
]

{ #category : 'testing' }
MetacelloVersionSpec >> isLoadedMatchConstraintsAgainst: resolvedPackageAndProjectNames [
	"all loaded projects and packages match constraints (at least one package loaded)"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isLoadedMatchConstraints: [ :ignored | ^ true ].
	^ false
]

{ #category : 'testing' }
MetacelloVersionSpec >> isLoadedToSpec [
	"all loaded projects and packages match specifications (at least one package loaded)"
	
	self isPartiallyCurrent isLoadedToSpec: [ :ignored | ^ true ].
	^ false
]

{ #category : 'testing' }
MetacelloVersionSpec >> isPartiallyCurrent [

	^self isPartiallyCurrentAgainst: (self expandToLoadableSpecNames: #('ALL'))
]

{ #category : 'testing' }
MetacelloVersionSpec >> isPartiallyCurrent: notLoadedMatters useEquality: useEquality [

	self subclassResponsibility
]

{ #category : 'testing' }
MetacelloVersionSpec >> isPartiallyCurrentAgainst: resolvedPackageAndProjectNames [
	| mcLoader status |
	status := MetacelloMCPartiallyLoadedStatus new.
	mcLoader := self loader.
	self
		specsNamed: resolvedPackageAndProjectNames
		projectDo: [ :prj | 
			| vrsn currentVersion |
			status hasNoProject: false.
			vrsn := prj versionOrNil.
			vrsn ~~ nil
				ifTrue: [ 
					(currentVersion := prj relativeCurrentVersion) ~~ nil
						ifTrue: [ status vrsnStatus add: currentVersion versionStatus ] ].
			currentVersion ~~ nil
				ifTrue: [ 
					status aProjectIsLoaded: true.
					(currentVersion perform: #= with: vrsn)
						ifTrue: [ status aLoadedProjectIsExact: true ]
						ifFalse: [ 
							(currentVersion perform: prj projectReference operator with: vrsn)
								ifTrue: [ status aLoadedProjectIsCurrent: true ]
								ifFalse: [ status aLoadedProjectIsNotCurrent: true ] ] ]
				ifFalse: [ status aProjectNotLoaded: true ] ]
		packageDo: [ :pkg | 
			status hasNoPackage: false.
			pkg
				currentPackageLoaded: [ :versionInfos :file | 
					| wcName wcRef fileRef exact current |
					status aPackageIsLoaded: true.
					versionInfos isEmpty
						ifTrue: [ status aLoadedPackageIsNotCurrent: true ]
						ifFalse: [ 
							exact := current := false.
							versionInfos
								do: [ :vi | 
									wcName := vi name.
									fileRef := GoferResolvedReference name: file.
									wcRef := GoferResolvedReference name: wcName.
									(wcRef compare: fileRef using: #=)
										ifTrue: [ exact := true ] ].
							exact
								ifTrue: [ status aLoadedPackageIsExact: true ]
								ifFalse: [ 
									versionInfos
										do: [ :vi | 
											wcName := vi name.
											fileRef := GoferResolvedReference name: file.
											wcRef := GoferResolvedReference name: wcName.
											(wcRef compare: fileRef using: #>=)
												ifTrue: [ current := true ] ].
									current
										ifTrue: [ status aLoadedPackageIsCurrent: true ]
										ifFalse: [ status aLoadedPackageIsNotCurrent: true ] ] ] ]
				notLoaded: [ status aPackageNotLoaded: true ]
				using: mcLoader ]
		groupDo: [ :ignoredGroup | 
			"if we encounter a group, trouble"
			status abort: true.
			^ status ].
	^ status
]

{ #category : 'testing' }
MetacelloVersionSpec >> isPossibleBaseline [

	self projectDo: [ :prj | prj isPossibleBaseline ifFalse: [ ^ false ] ] packageDo: [ :pkg | pkg isPackageLoaded ifFalse: [ ^ false ] ] groupDo: [ :ignored |  ].
	^ true
]

{ #category : 'testing' }
MetacelloVersionSpec >> isSomethingLoaded [
	"at least one package loaded"
	
	self isPartiallyCurrent isSomethingLoaded: [ :ignored | ^ true ].
	^ false
]

{ #category : 'printing' }
MetacelloVersionSpec >> label [

	^self versionString, ' [', self projectLabel, ']'
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

{ #category : 'construction' }
MetacelloVersionSpec >> package: aString constructor: aVersionConstructor [
    aVersionConstructor packageForVersion: aString
]

{ #category : 'construction' }
MetacelloVersionSpec >> package: aString overrides: aBlock constructor: aVersionConstructor [
    aVersionConstructor packageForVersion: aString overrides: aBlock
]

{ #category : 'construction' }
MetacelloVersionSpec >> package: packageName with: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor packageForVersion: packageName with: aBlockOrString
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNamed: aString [

	^self packageNamed: aString ifAbsent: [ nil ]
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNamed: aString forLoad: ignored forMap: map ifAbsent: absentBlock [
  "at the end of the day, I don't the forLoad arg is important ... 
   it just ends up triggering an error ... when during a real load, you would 
   not get error. After a decent period of mourning we should just scrub this 
   arg from system"

  | importSpec |
  ^ map
    at: aString
    ifAbsent: [ 
      (self importArray notNil or: [ self import notNil ])
        ifTrue: [ 
          "expect the 'missing' name to be satisfied within context of imported project"
          importArray
            ifNotNil: [ 
              importArray
                do: [ :assoc | 
                  ((assoc value includes: aString) and: [ map includesKey: assoc key ])
                    ifTrue: [ 
                      importSpec := (map at: assoc key)
                        mergeImportLoads: {aString};
                        yourself ] ].
              importSpec ifNotNil: [ ^ importSpec ] ].
          (importSpec isNil and: [ self import notNil ])
            ifTrue: [ 
              ^ (map at: self import ifAbsent: absentBlock)
                mergeImportLoads: {aString};
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
MetacelloVersionSpec >> packageNamed: aString forMap: map ifAbsent: absentBlock [
    "import: only allowed to be used with baseline project specs"

    ^ self
        packageNamed: aString
        forLoad: true
        forMap: map
        ifAbsent: absentBlock
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
  importNames := (packageNames := (packageMap values
    collect: [ :pkg | pkg name ]) asSet) copy.
  (self import isNil and: [ self importArray isNil ])
    ifTrue: [ ^ loadOrder select: [ :pkg | packageNames includes: pkg name ] ].
  loadOrder do: [ :pkg | importNames remove: pkg name ifAbsent: [  ] ].
  pkgs := OrderedCollection new.
  importProjectSpecs := Dictionary new.
  importProjectNameMap := Dictionary new.
  importArray
    ifNotNil: [ 
      loadOrder
        do: [ :pkg | 
          importArray
            do: [ :assoc | 
              assoc key = pkg name
                ifTrue: [ 
                  importProjectSpecs at: pkg name put: pkg.
                  (assoc value select: [ :each | importNames includes: each ])
                    do: [ :each | 
                      (importProjectNameMap
                        at: pkg name
                        ifAbsent: [ importProjectNameMap at: pkg name put: Set new ])
                        add: each ] ] ] ] ].
  self import
    ifNotNil: [ 
      loadOrder
        do: [ :pkg | 
          pkg name = self import
            ifTrue: [ 
              importProjectSpecs at: pkg name put: pkg.
              importProjectNameMap at: pkg name put: importNames ] ] ].
  loadOrder
    do: [ :pkg | 
      (packageNames includes: pkg name)
        ifTrue: [ pkgs add: pkg ].
      importProjectSpecs
        at: pkg name
        ifPresent: [ :importProjectSpec | 
          "insert the imports at this point"
          (importProjectNameMap at: pkg name ifAbsent: [ #() ])
            do: [ :importedName | 
              pkgs
                add:
                  (importSpec := importProjectSpec copy
                    name: importedName;
                    mergeImportLoads: {importedName};
                    yourself).
              importSpec projectReference name: importedName ] ] ].
  ^ pkgs
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

{ #category : 'construction' }
MetacelloVersionSpec >> postLoadDoIt: aSymbol constructor: aVersionConstructor [
    aVersionConstructor postLoadDoItForVersion: aSymbol
]

{ #category : 'querying' }
MetacelloVersionSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> preLoadDoIt: anObject [

	anObject setPreLoadDoItInMetacelloSpec: self
]

{ #category : 'construction' }
MetacelloVersionSpec >> preLoadDoIt: aSymbol constructor: aVersionConstructor [
    aVersionConstructor preLoadDoItForVersion: aSymbol
]

{ #category : 'construction' }
MetacelloVersionSpec >> project: aString constructor: aVersionConstructor [
    aVersionConstructor projectForVersion: aString
]

{ #category : 'construction' }
MetacelloVersionSpec >> project: aString copyFrom: oldSpecName with: aBlock constructor: aVersionConstructor [
    aVersionConstructor projectForVersion: aString copyFrom: oldSpecName with: aBlock
]

{ #category : 'construction' }
MetacelloVersionSpec >> project: aString overrides: aBlock constructor: aVersionConstructor [
    aVersionConstructor projectForVersion: aString overrides: aBlock
]

{ #category : 'construction' }
MetacelloVersionSpec >> project: aString with: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor projectForVersion: aString with: aBlockOrString
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	self packageSpecsInLoadOrder do: [:pkgSpec |
		pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> projectLabel [

	^self project label
]

{ #category : 'construction' }
MetacelloVersionSpec >> removeGroup: aString constructor: aVersionConstructor [
    aVersionConstructor removeGroupForVersion: aString
]

{ #category : 'construction' }
MetacelloVersionSpec >> removePackage: aString constructor: aVersionConstructor [
    aVersionConstructor removePackageForVersion: aString
]

{ #category : 'construction' }
MetacelloVersionSpec >> removeProject: aString constructor: aVersionConstructor [
    aVersionConstructor removeProjectForVersion: aString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories [

	^ repositories ifNil: [ repositories := self project repositoriesSpec ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories: anObject [
	repositories := anObject
]

{ #category : 'construction' }
MetacelloVersionSpec >> repositories: aBlock constructor: aVersionConstructor [
    aVersionConstructor repositoriesForVersion: aBlock
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositoriesSpec [

	^self repositories
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString [
	self repositoriesSpec add: aString
]

{ #category : 'construction' }
MetacelloVersionSpec >> repository: anObject constructor: aVersionConstructor [
    aVersionConstructor repositoryForVersion: anObject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString username: username password: password [
	self repositoriesSpec repository: aString username: username password: password
]

{ #category : 'construction' }
MetacelloVersionSpec >> repository: aString username: username password: password constructor: aVersionConstructor [
    aVersionConstructor repositoryForVersion: aString username: username password: password
]

{ #category : 'loading' }
MetacelloVersionSpec >> repositorySpecs [

	^self repositories map values
]

{ #category : 'loading' }
MetacelloVersionSpec >> resolveToLoadableSpec: aString forLoad: forLoad forMap: map packages: packageMap [
    | package |
    package := self
        packageNamed: aString
        forLoad: forLoad
        forMap: map
        ifAbsent: [ ^ self error: 'Name not found: ' , aString ].
    packageMap at: package name put: package.
    ^ {package}
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
  self resolveToLoadableSpecs: nameList forLoad: false map: map.
  ^ map values
]

{ #category : 'loading' }
MetacelloVersionSpec >> resolveToLoadableSpecs: required forLoad: forLoad map: packageMap [
  | reqd allReqd map newReqd spec |
  reqd := required copy.
  allReqd := Set new.
  map := self packages map.
  [ reqd isEmpty ]
    whileFalse: [ 
      newReqd := Set new.
      reqd
        do: [ :req | 
          (self
            resolveToLoadableSpec: req
            forLoad: forLoad
            forMap: map
            packages: packageMap)
            do: [ :loadableSpec | 
              newReqd addAll: loadableSpec requires.
              newReqd addAll: loadableSpec includes ] ].
      allReqd addAll: reqd.
      newReqd removeAllFoundIn: allReqd.
      reqd := newReqd ].
  packageMap keys
    do: [ :pkgName | 
      (spec := (packageMap at: pkgName) resolveToLoadableSpec) == nil
        ifTrue: [ packageMap removeKey: pkgName ]
        ifFalse: [ packageMap at: pkgName put: (packageMap at: pkgName) resolveToLoadableSpec ] ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> setAuthor: anObject [
	author := anObject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> setBlessing: anObject [
	blessing := anObject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> setDescription: anObject [
	description := anObject
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

{ #category : 'accessing' }
MetacelloVersionSpec >> setTimestamp: anObject [
	timestamp := anObject
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
	packageAndProjectNames do: [:name | | pkgSpec |
		(pkgSpec := map at: name ifAbsent: [ ]) ~~ nil
			ifTrue: [ pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]]
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

	anObject setTimestampInMetacelloVersion: self
]

{ #category : 'construction' }
MetacelloVersionSpec >> timestamp: aBlockOrStringOrDateAndTime constructor: aVersionConstructor [
    aVersionConstructor timestampForVersion: aBlockOrStringOrDateAndTime
]

{ #category : 'development support' }
MetacelloVersionSpec >> updateForSpawnMethod: sourceSpec [
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"
	
	repositories := preLoadDoIt := postLoadDoIt := nil.
]

{ #category : 'development support' }
MetacelloVersionSpec >> updatedPackageSpecs [

	| updatedSpecs mcLoader |
	updatedSpecs := Dictionary new.
	mcLoader := self loader.
	self packages map valuesDo: [:pkg | pkg updatePackageSpec: updatedSpecs using: mcLoader].
	^updatedSpecs
]

{ #category : 'private' }
MetacelloVersionSpec >> versionClass [

	^MetacelloVersion
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

{ #category : 'construction' }
MetacelloVersionSpec >> versionString: anObject constructor: aVersionConstructor [
    aVersionConstructor versionStringForVersion: anObject
]