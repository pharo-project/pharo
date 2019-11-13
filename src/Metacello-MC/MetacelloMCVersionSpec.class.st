Class {
	#name : #MetacelloMCVersionSpec,
	#superclass : #MetacelloVersionSpec,
	#instVars : [
		'repositories',
		'packages'
	],
	#category : #'Metacello-MC-Specs'
}

{ #category : #accessing }
MetacelloMCVersionSpec >> computeVersionStatus: matchBlock [
	self computeVersionStatus: (self expandToLoadableSpecNames: #('ALL')) matchBlock: matchBlock
]

{ #category : #accessing }
MetacelloMCVersionSpec >> computeVersionStatus: resolvedPackageAndProjectNames matchBlock: matchBlock [

	| status |

	status := resolvedPackageAndProjectNames
		ifNil: [ self isPartiallyCurrent ]
		ifNotNil: [ self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames ].
	status isAllLoadedToSpec: matchBlock.
	status isLoadedToSpec: matchBlock.
	status isLoadedMatchConstraints: matchBlock.
	status isSomethingLoaded: matchBlock
]

{ #category : #printing }
MetacelloMCVersionSpec >> configMethodOn: aStream last: last indent: indent [
    | spec hasRepositories hasPackageSpecs hasImport |
    hasRepositories := (spec := self repositoriesSpec) ~~ nil and: [ spec list isEmpty not ].
    hasImport := self import ~~ nil.
    hasPackageSpecs := false.
    self packagesSpec list
        do: [ :member | 
            member spec
                projectDo: [ :proj | 
                    member spec name ~~ nil
                        ifTrue: [ hasPackageSpecs := true ] ]
                packageDo: [ :package | 
                    member spec name ~~ nil
                        ifTrue: [ hasPackageSpecs := true ] ]
                groupDo: [ :group | 
                    member spec name ~~ nil
                        ifTrue: [ hasPackageSpecs := true ] ] ].
    self configMethodBasicOn: aStream last: (hasRepositories | hasPackageSpecs | hasImport) not indent: indent.
    hasImport
        ifTrue: [ 
            self
                configMethodValueOn: aStream
                for: self import
                selector: 'import:'
                last: (hasRepositories | hasPackageSpecs) not
                indent: indent ].
    hasRepositories
        ifTrue: [ 
            spec map values size = 1
                ifTrue: [ 
                    aStream
                        tab: indent;
                        nextPutAll: 'spec repository: ';
                        nextPutAll: spec map values first description printString , '.'.
                    hasPackageSpecs
                        ifTrue: [ aStream cr ] ]
                ifFalse: [ 
                    self
                        configMethodOn: aStream
                        for: spec
                        selector: 'repositories:'
                        last: hasPackageSpecs not
                        indent: indent ] ].
    self configPackagesSpecMethodOn: aStream indent: indent.
    last
        ifFalse: [ aStream cr ]
]

{ #category : #printing }
MetacelloMCVersionSpec >> configSpawnMethodOn: aStream indent: indent [

	super configSpawnMethodOn: aStream indent: indent.
	self configPackagesSpecMethodOn: aStream indent: indent.
]

{ #category : #querying }
MetacelloMCVersionSpec >> currentlyLoadedClassesInVersion [
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
        ifTrue: [ classes addAll: (MetacelloPlatform current packageInfoFor: wc) classes ] ]
    groupDo: [ :ignored |  ].
  ^ classes
]

{ #category : #querying }
MetacelloMCVersionSpec >> currentlyLoadedExtensionClassesInVersion [
	| classes |
	classes := Dictionary new.
	self
		projectDo: [ :ignored |  ]
		packageDo: [ :packageSpec | 
			([ packageSpec workingCopy ]
				on: Error
				do: [ :ex | ex return: nil ])
				ifNotNil: [ :workingCopy | 
					| packageInfo |
					packageInfo := MetacelloPlatform current packageInfoFor: workingCopy.
					packageInfo extendedClasses do: [ :cl | classes at: cl put: (packageInfo extensionCategoriesForClass: cl) ] ] ]
		groupDo: [ :ignored |  ].
	^ classes
]

{ #category : #querying }
MetacelloMCVersionSpec >> difference: otherVersionSpec [
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
    myProjectSpecs
        valuesDo: [ :myProjectSpec | 
            | otherProjectSpec |
            otherProjectSpec := otherProjectSpecs at: myProjectSpec name ifAbsent: [  ].
            otherProjectSpec == nil
                ifTrue: [ 
                    report removals
                        at: myProjectSpec name
                        put:
                            {(myProjectSpec versionString).
                            ''} ]
                ifFalse: [ 
                    myProjectSpec versionString = otherProjectSpec versionString
                        ifFalse: [ 
                            report modifications
                                at: myProjectSpec name
                                put:
                                    {(myProjectSpec versionString).
                                    (otherProjectSpec versionString)} ] ] ].
    otherProjectSpecs
        valuesDo: [ :otherProjectSpec | 
            (myProjectSpecs at: otherProjectSpec name ifAbsent: [  ]) == nil
                ifTrue: [ 
                    report additions
                        at: otherProjectSpec name
                        put:
                            {''.
                            (otherProjectSpec versionString)} ] ].
    myPackageSpecs
        valuesDo: [ :myPackageSpec | 
            | otherPackageSpec |
            otherPackageSpec := otherPackageSpecs at: myPackageSpec name ifAbsent: [  ].
            otherPackageSpec == nil
                ifTrue: [ 
                    report removals
                        at: myPackageSpec name
                        put:
                            {(myPackageSpec file).
                            ''} ]
                ifFalse: [ 
                    myPackageSpec file = otherPackageSpec file
                        ifFalse: [ 
                            report modifications
                                at: myPackageSpec name
                                put:
                                    {(myPackageSpec file).
                                    (otherPackageSpec file)} ] ] ].
    otherPackageSpecs
        valuesDo: [ :otherPackageSpec | 
            (myPackageSpecs at: otherPackageSpec name ifAbsent: [  ]) == nil
                ifTrue: [ 
                    report additions
                        at: otherPackageSpec name
                        put:
                            {''.
                            (otherPackageSpec file)} ] ].
    ^ report
]

{ #category : #'development support' }
MetacelloMCVersionSpec >> forceUpdatedPackageSpecs [

	| updatedSpecs mcLoader |
	updatedSpecs := Dictionary new.
	mcLoader := self loader.
	self packages map valuesDo: [:pkg | pkg forceUpdatePackageSpec: updatedSpecs using: mcLoader].
	^updatedSpecs
]

{ #category : #testing }
MetacelloMCVersionSpec >> isAllLoadedToSpec [
	"all projects and packages are loaded and match specification"

	self isPartiallyCurrent isAllLoadedToSpec: [ :ignored | ^ true ].
	^ false
]

{ #category : #testing }
MetacelloMCVersionSpec >> isAllLoadedToSpecAgainst: resolvedPackageAndProjectNames [
	"all projects and packages are loaded and match specification"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isAllLoadedToSpec: [ :ignored | ^ true ].
	^ false
]

{ #category : #testing }
MetacelloMCVersionSpec >> isLoadedMatchConstraints [
	"all loaded projects and packages match constraints (at least one package loaded)"
	
	self isPartiallyCurrent isLoadedMatchConstraints: [ :ignored | ^ true ].
	^ false
]

{ #category : #testing }
MetacelloMCVersionSpec >> isLoadedMatchConstraintsAgainst: resolvedPackageAndProjectNames [
	"all loaded projects and packages match constraints (at least one package loaded)"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isLoadedMatchConstraints: [ :ignored | ^ true ].
	^ false
]

{ #category : #testing }
MetacelloMCVersionSpec >> isLoadedToSpec [
	"all loaded projects and packages match specifications (at least one package loaded)"
	
	self isPartiallyCurrent isLoadedToSpec: [ :ignored | ^ true ].
	^ false
]

{ #category : #testing }
MetacelloMCVersionSpec >> isLoadedToSpecAgainst: resolvedPackageAndProjectNames [
	"all loaded projects and packages match specifications (at least one package loaded)"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isLoadedToSpec: [ :ignored | ^ true ].
	^ false
]

{ #category : #testing }
MetacelloMCVersionSpec >> isPartiallyCurrent [

	^self isPartiallyCurrentAgainst: (self expandToLoadableSpecNames: #('ALL'))
]

{ #category : #testing }
MetacelloMCVersionSpec >> isPartiallyCurrentAgainst: resolvedPackageAndProjectNames [
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

{ #category : #testing }
MetacelloMCVersionSpec >> isPossibleBaseline [

	self 
		projectDo: [:prj | prj isPossibleBaseline ifFalse: [ ^false ]] 
		packageDo: [:pkg | pkg isPackageLoaded ifFalse: [ ^false ]] 
		groupDo: [:ignored | ].
	^true
]

{ #category : #testing }
MetacelloMCVersionSpec >> isSomethingLoaded [
	"at least one package loaded"
	
	self isPartiallyCurrent isSomethingLoaded: [ :ignored | ^ true ].
	^ false
]

{ #category : #testing }
MetacelloMCVersionSpec >> isSomethingLoadedAgainst: resolvedPackageAndProjectNames [
	"at least one package loaded"

	(self isPartiallyCurrentAgainst: resolvedPackageAndProjectNames) isSomethingLoaded: [ :ignored | ^ true ].
	^ false
]

{ #category : #merging }
MetacelloMCVersionSpec >> mergeMap [
    | map |
    map := super mergeMap.
    map at: #'repositories' put: self repositories.
    ^ map
]

{ #category : #merging }
MetacelloMCVersionSpec >> mergeSpec: anotherSpec [
    | newSpec map anotherRepositories |
    newSpec := super mergeSpec: anotherSpec.
    map := anotherSpec mergeMap.
    (anotherRepositories := map at: #'repositories') isEmpty not
        ifTrue: [ 
            newSpec
                repositories:
                    (self repositories isEmpty
                        ifTrue: [ anotherRepositories ]
                        ifFalse: [ self repositories mergeSpec: anotherRepositories ]) ].
    ^ newSpec
]

{ #category : #merging }
MetacelloMCVersionSpec >> nonOverridable [
    ^ super nonOverridable , #(#'repositories')
]

{ #category : #construction }
MetacelloMCVersionSpec >> package: aString constructor: aVersionConstructor [
    aVersionConstructor packageForVersion: aString
]

{ #category : #construction }
MetacelloMCVersionSpec >> package: aString overrides: aBlock constructor: aVersionConstructor [
    aVersionConstructor packageForVersion: aString overrides: aBlock
]

{ #category : #construction }
MetacelloMCVersionSpec >> package: packageName with: aBlockOrString constructor: aVersionConstructor [
    aVersionConstructor packageForVersion: packageName with: aBlockOrString
]

{ #category : #querying }
MetacelloMCVersionSpec >> packageNames [
    "leave reference to packages for upgrade purposes"

    packages == nil
        ifTrue: [ ^ super packageNames ].
    ^ self packages map keys asSet
]

{ #category : #querying }
MetacelloMCVersionSpec >> packageSpecsInLoadOrderForMap: packageMap [
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

{ #category : #accessing }
MetacelloMCVersionSpec >> packages [
    "leave reference to packages for upgrade purposes"

    packages == nil
        ifTrue: [ ^ super packages ].
    ^ packages
]

{ #category : #'development support' }
MetacelloMCVersionSpec >> packagesNeedSavingVisited: visitedProjects into: aCollection [

	self packages map valuesDo: [:pkg | pkg packagesNeedSavingVisited: visitedProjects using: self repositories map values into: aCollection ].
]

{ #category : #copying }
MetacelloMCVersionSpec >> postCopy [
    super postCopy.
    repositories := repositories copy.
    packages := packages copy	"leave reference to packages for upgrade purposes"
]

{ #category : #construction }
MetacelloMCVersionSpec >> removePackage: aString constructor: aVersionConstructor [
    aVersionConstructor removePackageForVersion: aString
]

{ #category : #accessing }
MetacelloMCVersionSpec >> repositories [
	repositories == nil ifTrue: [ repositories := self project repositoriesSpec ].
	^ repositories
]

{ #category : #accessing }
MetacelloMCVersionSpec >> repositories: anObject [
	repositories := anObject
]

{ #category : #accessing }
MetacelloMCVersionSpec >> repositoriesSpec [

	^self repositories
]

{ #category : #accessing }
MetacelloMCVersionSpec >> repository: aString [
	self repositoriesSpec add: aString
]

{ #category : #accessing }
MetacelloMCVersionSpec >> repository: aString username: username password: password [
	self repositoriesSpec repository: aString username: username password: password
]

{ #category : #loading }
MetacelloMCVersionSpec >> repositorySpecs [

	^self repositories map values
]

{ #category : #enumerating }
MetacelloMCVersionSpec >> specListProjectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	self packages specListDo: [:pkgSpec |
		pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]
]

{ #category : #enumerating }
MetacelloMCVersionSpec >> specsNamed: packageAndProjectNames projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	| map |
	map := self packages map.
	packageAndProjectNames do: [:name | | pkgSpec |
		(pkgSpec := map at: name ifAbsent: [ ]) ~~ nil
			ifTrue: [ pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]]
]

{ #category : #'development support' }
MetacelloMCVersionSpec >> updateForSpawnMethod: sourceSpec [
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"
	
	repositories := preLoadDoIt := postLoadDoIt := nil.
]

{ #category : #'development support' }
MetacelloMCVersionSpec >> updatedPackageSpecs [

	| updatedSpecs mcLoader |
	updatedSpecs := Dictionary new.
	mcLoader := self loader.
	self packages map valuesDo: [:pkg | pkg updatePackageSpec: updatedSpecs using: mcLoader].
	^updatedSpecs
]

{ #category : #private }
MetacelloMCVersionSpec >> versionClass [

	^MetacelloMCVersion
]
