"
An MCConfiguration specifies the configuration of a set of related Monticello packages. It maintains an ordered list of package versions and a list of repositories in which the packages may be found.

An MCConfiguration may be filed out for storage as an array specification, and new instances can be created from a stored array specification.

"
Class {
	#name : #MCConfiguration,
	#superclass : #Object,
	#instVars : [
		'name',
		'dependencies',
		'repositories',
		'log'
	],
	#classVars : [
		'DefaultLog',
		'UpgradeIsMerge'
	],
	#category : #MonticelloConfigurations
}

{ #category : #utilities }
MCConfiguration class >> changesIn: aPackage from: oldInfo to: newInfo on: aStream [
	| printBlock newVersion |
	(newInfo = oldInfo)
		ifTrue: [^self].
	aStream cr; nextPutAll: '----------------- ', aPackage name, ' ------------------'; cr.

	newInfo 
		ifNil: [^aStream cr; nextPutAll: 'REMOVED'; cr].

	oldInfo 
		ifNil: [^aStream cr; nextPutAll: 'ADDED'; cr].

	"get actual version for full ancestry"
	newVersion := MCRepositoryGroup default versionWithInfo: newInfo.

	printBlock := [:ver | 
		aStream cr; 
			nextPutAll: (ver name copyAfterLast: $-);
			nextPutAll:  ' (', (ver date printFormat: #(1 2 0 $. 1 1 2)), ', '.
		ver time print24: true showSeconds: false on: aStream.
		aStream nextPutAll: ')'; cr; 
			nextPutAll: ver message; cr].

	(newVersion info hasAncestor: oldInfo)
		ifTrue: [(newVersion info allAncestorsOnPathTo: oldInfo)
			reverseDo: printBlock].
	newVersion info in: printBlock.
	aStream flush
]

{ #category : #accessing }
MCConfiguration class >> defaultLog [
	"Answer the default configuration log"
	^DefaultLog
]

{ #category : #accessing }
MCConfiguration class >> defaultLog: aStream [
	"Set the default configuration log"
	DefaultLog := aStream.
]

{ #category : #converting }
MCConfiguration class >> dependencyFromArray: anArray [
	^MCVersionDependency
		package: (MCPackage named: anArray first)
		info: (
			MCVersionInfo
			name: anArray second
			id: (UUID fromString: anArray third)
			message: nil
			date: nil
			time: nil
			author: nil
			ancestors: nil)
]

{ #category : #converting }
MCConfiguration class >> dependencyToArray: aDependency [
	^ {
		aDependency package name . 
		aDependency versionInfo name . 
		aDependency versionInfo id asString }
]

{ #category : #'instance creation' }
MCConfiguration class >> fromArray: anArray [
	| configuration |
	configuration := self new.
	anArray pairsDo: [:key :value |
		key = #repository
			ifTrue: [configuration repositories add: (self repositoryFromArray: value)].
		key = #dependency
			ifTrue: [configuration dependencies add: (self dependencyFromArray: value)].
	].
	^configuration
]

{ #category : #converting }
MCConfiguration class >> repositoryFromArray: anArray [
	^ MCRepositoryGroup default repositories
		detect: [:repo | repo description = anArray first]
		ifNone: [
			MCHttpRepository
				location: anArray first
				user: ''
				password: '']
]

{ #category : #converting }
MCConfiguration class >> repositoryToArray: aRepository [
	^ {aRepository description}
]

{ #category : #settings }
MCConfiguration class >> upgradeIsMerge [
	^ UpgradeIsMerge ifNil: [UpgradeIsMerge := false]
]

{ #category : #settings }
MCConfiguration class >> upgradeIsMerge: aBoolean [
	UpgradeIsMerge := aBoolean
]

{ #category : #utilities }
MCConfiguration class >> whatChangedFrom: oldConfig to: newConfig [
	"MCConfiguration
		whatChangedFrom: ReleaseBuilderPloppDeveloper config20060201PloppBeta
		to:  ReleaseBuilderPloppDeveloper config20060215premaster"

	self
		whatChangedFrom: oldConfig
		to: newConfig
		on: Smalltalk tools transcript
]

{ #category : #utilities }
MCConfiguration class >> whatChangedFrom: oldConfig to: newConfig on: aStream [
	"MCConfiguration
		whatChangedFrom:  ReleaseBuilderPloppDeveloper config20060201PloppBeta
		to:  ReleaseBuilderPloppDeveloper config20060215premaster" 

	| oldDeps |
	oldDeps := Dictionary new.
	oldConfig dependencies do: [:old | oldDeps at: old package put: old].

	newConfig dependencies do: [:new | | old |
		old := oldDeps removeKey: new package ifAbsent: [nil].
		old ifNotNil: [old := old versionInfo].
		self changesIn: new package from: old to: new versionInfo on: aStream.
	].

	oldDeps do: [:old |
		self changesIn: old package from: old versionInfo to: nil on: aStream.
	].

]

{ #category : #faking }
MCConfiguration >> changes [
	^MCPatch operations: #()
]

{ #category : #accessing }
MCConfiguration >> dependencies [
	^dependencies ifNil: [dependencies := OrderedCollection new]
]

{ #category : #accessing }
MCConfiguration >> dependencies: aCollection [
	dependencies := aCollection
]

{ #category : #private }
MCConfiguration >> depsSatisfying: selectBlock versionDo: verBlock displayingProgress: progressString [
	| repoMap count |
	repoMap := Dictionary new.
	self repositories do: [:repo |
		MCRepositoryGroup default addRepository: repo.
		repo allVersionNames
			ifEmpty: [self logWarning: 'cannot read from ', repo description]
			ifNotEmpty: [:all | all do: [:ver | repoMap at: ver put: repo]]].

	count := 0.
	self dependencies do: [:dep |
		| ver repo |
		ver := dep versionInfo name.
		repo := repoMap 
					at: ver 
					ifAbsent: 
						[self logError: 'Version ', ver, ' not found in any repository'.
						self logError: 'Aborting'.
						^count].
		(selectBlock value: dep) 
			ifTrue: [| new |
					new := self versionNamed: ver for: dep from: repo.
					new ifNil: 
							[self logError: 'Could not download version ', ver, ' from ', repo description.
							self logError: 'Aborting'.
							^count]
						ifNotNil: 
							[ self logUpdate: dep package with: new.
							ProgressNotification signal: '' extra: 'Installing ', ver.
							verBlock value: new.
							count := count + 1]].
		dep package workingCopy repositoryGroup addRepository: repo.
	] displayingProgress: progressString.
	^count
]

{ #category : #private }
MCConfiguration >> diffBaseFor: aDependency [
	| wc |
	aDependency package hasWorkingCopy ifFalse: [^nil].
	wc := aDependency package workingCopy.
	wc ancestors ifEmpty: [^nil].
	^wc ancestors first name
]

{ #category : #accessing }
MCConfiguration >> fileName [
	^ self name, '.', self writerClass extension

]

{ #category : #actions }
MCConfiguration >> fileOutOn: aStream [
	self writerClass fileOut: self on: aStream
]

{ #category : #faking }
MCConfiguration >> info [
	^MCVersionInfo new
]

{ #category : #initialization }
MCConfiguration >> initialize [
	super initialize.
	log := DefaultLog.
]

{ #category : #testing }
MCConfiguration >> isCacheable [
	^false
]

{ #category : #actions }
MCConfiguration >> load [
	"install all the versions in this configuration, even if this means to downgrade a package"

	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isCurrent not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^self loadVersions: versions
]

{ #category : #private }
MCConfiguration >> loadVersions: aCollection [

	| loader |
	aCollection isEmpty ifTrue: [^0].
	loader := MCVersionLoader new.
	aCollection do: [:each | loader addVersion: each].
	loader loadWithNameLike: self nameForChangeset.
	^ aCollection size
]

{ #category : #accessing }
MCConfiguration >> log [
	^log ifNil: [Transcript]
]

{ #category : #accessing }
MCConfiguration >> log: aStream [
	log := aStream.
]

{ #category : #logging }
MCConfiguration >> logError: aString [
	self log
		cr; nextPutAll: 'ERROR: ';
		nextPutAll: aString; cr;
		flush.

]

{ #category : #logging }
MCConfiguration >> logUpdate: aPackage with: aVersion [
	self log
		cr; nextPutAll: ('========== <1s> ==========' expandMacrosWith:aVersion info name); cr;
		cr; nextPutAll: aVersion info message asString; cr;
		flush.

	aPackage hasWorkingCopy ifFalse: [^self].

	aPackage workingCopy ancestors do: [:each |
		(aVersion info hasAncestor: each)
			ifTrue: [(aVersion info allAncestorsOnPathTo: each)
				do: [:ver | self log cr; nextPutAll: ('>>> <1s> <<<' expandMacrosWith:ver name) ; cr;
							nextPutAll: ver message; cr; flush]]]
]

{ #category : #logging }
MCConfiguration >> logWarning: aString [
	self log
		cr; nextPutAll: 'WARNING: ';
		nextPutAll: aString; cr;
		flush.

]

{ #category : #actions }
MCConfiguration >> merge [
	"merge in all the versions in this configuration"

	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isFulfilledByAncestors not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^self mergeVersions: versions
]

{ #category : #private }
MCConfiguration >> mergeVersions: aCollection [

	| merger |
	aCollection isEmpty ifTrue: [^0].
	merger := MCVersionMerger new.
	aCollection do: [:each | merger addVersion: each].
	merger mergeWithNameLike: self nameForChangeset.
	^ aCollection size
]

{ #category : #private }
MCConfiguration >> mergeVersionsSilently: aCollection [

	^self suppressMergeDialogWhile: [self mergeVersions: aCollection]
]

{ #category : #private }
MCConfiguration >> mustMerge: aVersion [
	"answer true if we have to do a full merge and false if we can simply load instead"
	
	| pkg wc current |
	(pkg := aVersion package) hasWorkingCopy ifFalse: [^false "no wc -> load"].
	(wc := pkg workingCopy) modified ifTrue: [^true "modified -> merge"].
	wc ancestors isEmpty ifTrue: [^true "no ancestor info -> merge"].
	current := wc ancestors first.
	(aVersion info hasAncestor: current) ifTrue: [^false "direct descendant of wc -> load"].
	"new branch -> merge"
	^true
]

{ #category : #accessing }
MCConfiguration >> name [
	^name
]

{ #category : #accessing }
MCConfiguration >> name: aString [
	name := aString
]

{ #category : #private }
MCConfiguration >> nameForChangeset [
	^self name ifNil: [self class name]
]

{ #category : #copying }
MCConfiguration >> postCopy [
	dependencies := dependencies shallowCopy.
	repositories := repositories shallowCopy.
]

{ #category : #accessing }
MCConfiguration >> repositories [
	^repositories ifNil: [repositories := OrderedCollection new]
]

{ #category : #accessing }
MCConfiguration >> repositories: aCollection [
	repositories := aCollection
]

{ #category : #accessing }
MCConfiguration >> summary [
	^String streamContents: [:stream |
		self dependencies
			do: [:ea | stream nextPutAll: ea versionInfo name; cr ]]
]

{ #category : #private }
MCConfiguration >> suppressMergeDialogWhile: aBlock [
	^aBlock 
		on: MCMergeResolutionRequest
		do: [:request | request autoMerge ]
]

{ #category : #updating }
MCConfiguration >> updateFromImage [
	self dependencies: (self dependencies collect: [:dep |
		dep package hasWorkingCopy
			ifTrue: [
				dep package workingCopy in: [:wc |
					MCVersionDependency package: wc package info: wc ancestors first]]
			ifFalse: [dep]]).

]

{ #category : #updating }
MCConfiguration >> updateFromRepositories [
	| oldInfos newNames sortedNames newDeps |
	oldInfos := self dependencies collect: [:dep | dep versionInfo].
	newNames := Dictionary new.
	self repositories
		do: [ :repo | 
			ProgressNotification signal: '' extra: 'Checking ', repo description.
			(repo possiblyNewerVersionsOfAnyOf: oldInfos)
				do: [:newName | newNames at: newName put: repo]]
		displayingProgress: 'Searching new versions'.

	sortedNames := newNames keys asSortedCollection:
		[:a :b | a numericSuffix > b numericSuffix].

	newDeps := OrderedCollection new.
	self dependencies do: [:dep |
		| newName |
		newName := sortedNames
			detect: [:each | (each copyUpToLast: $-) = dep package name]
			ifNone: [nil].
		newDeps add: (newName
			ifNil: [dep]
			ifNotNil: [
				| repo ver  |
				repo := newNames at: newName.
				ver := self versionNamed: newName for: dep from: repo.
				ver ifNil: [dep]
					ifNotNil: [MCVersionDependency package: ver package info: ver info]
			])
	] displayingProgress: 'downloading new versions'.

	self dependencies: newDeps.

]

{ #category : #actions }
MCConfiguration >> upgrade [
	^self class upgradeIsMerge
		ifTrue: [self upgradeByMerging]
		ifFalse: [self upgradeByLoading]
]

{ #category : #actions }
MCConfiguration >> upgradeByLoading [
	"this differs from #load only in that newer versions in the image are not downgraded"

	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isFulfilledByAncestors not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^self loadVersions: versions

]

{ #category : #actions }
MCConfiguration >> upgradeByMerging [
	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isFulfilledByAncestors not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^(versions noneSatisfy: [:ver | self mustMerge: ver])
		ifTrue: [self loadVersions: versions]
		ifFalse: [self mergeVersionsSilently: versions].

]

{ #category : #private }
MCConfiguration >> versionNamed: verName for: aDependency from: repo [

	| baseName fileName ver |
	(repo filterFileNames: repo cachedFileNames forVersionNamed: verName) ifNotEmpty: [:cachedNames |
		fileName := cachedNames anyOne.
		ProgressNotification signal: '' extra: 'Using cached ', fileName.
		ver := repo versionFromFileNamed: fileName].
	ver ifNil: [
		baseName := self diffBaseFor: aDependency.
		(baseName notNil and: [baseName ~= verName and: [repo includesVersionNamed: baseName]]) ifTrue: [
			fileName := (MCDiffyVersion nameForVer: verName base: baseName), '.mcd'.
			ProgressNotification signal: '' extra: 'Downloading ', fileName.
			ver := repo versionFromFileNamed: fileName]].
	ver ifNil: [
		fileName := verName, '.mcz'.
		ProgressNotification signal: '' extra: 'Downloading ', fileName.
		ver := repo versionFromFileNamed: fileName].
	^ver
]

{ #category : #accessing }
MCConfiguration >> writerClass [
	^ MCMcmWriter 
]
