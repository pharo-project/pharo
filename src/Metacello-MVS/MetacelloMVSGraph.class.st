"
I am the facts resolution works on: the versions and dates of each repository, and the reading of the baseline of each version. What I was told (tests) is kept; anything else I ask my source (a MetacelloMVSVersionSource) once, and cache.

The source answers baseline definitions; I run them in a MetacelloMVSBaselineSandbox, checking the minimum Pharo against pharoMajor (this image's unless told), and keep the MetacelloMVSBaselineReading. I never signal: the baseline is third-party code, so whatever it fails with becomes a finding, and a repository whose versions cannot be read has no versions and a failure (failureOf:).
"
Class {
	#name : 'MetacelloMVSGraph',
	#superclass : 'Object',
	#instVars : [
		'dates',
		'source',
		'repositories',
		'versions',
		'projectVersions',
		'givenProjectVersions'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSGraph class >> new [
	self error: 'A graph needs a version source: use on:'
]

{ #category : 'instance creation' }
MetacelloMVSGraph class >> on: aVersionSource [
	"A graph asks aVersionSource the facts it was not given by hand"
	^ self basicNew initializeOn: aVersionSource
]

{ #category : 'reading' }
MetacelloMVSGraph >> causeOf: aCompilationError [
	"Why the baseline does not compile, in one line: what the compiler noticed"

	^ ', ' join: (aCompilationError compilationResult ast allNotices collect: [ :each | each messageText ])
]

{ #category : 'reading' }
MetacelloMVSGraph >> checkPlatformOf: aCandidate in: aSandbox into: aProjectVersion [
	"for: #pharoN.x blocks are accepted, with a warning: the requirements then depend on the image"

	aSandbox platformBlocks ifNotEmpty: [ :platforms | aProjectVersion warning: #platformBlock text: (', ' join: platforms) ]
]

{ #category : 'versions' }
MetacelloMVSGraph >> datesOf: aRepository [
	"Version name -> DateAndTime of each version of aRepository whose date is known. Asked once its versions are read (see versionsOf:ifFail:)"
	^ dates at: aRepository
]

{ #category : 'accessing' }
MetacelloMVSGraph >> description: aDescription requires: aCollectionOfProjectSpecs [
	"Given by hand: the baseline read from aDescription, a directory (tonel:///path), requires aCollectionOfProjectSpecs, and says nothing else"

	(self givenProjectVersionOf: aDescription) references: aCollectionOfProjectSpecs
]

{ #category : 'project specs' }
MetacelloMVSGraph >> descriptionOf: aProjectSpec [
	"The one repository description of aProjectSpec, as written (github://owner/name:ref/path)"

	^ aProjectSpec repositoryDescriptions first
]

{ #category : 'accessing' }
MetacelloMVSGraph >> givenProjectVersionOf: aDescription [
	"The project version given by hand for aDescription, a repository at a version or a directory, for whatever project asks it there"

	^ givenProjectVersions at: aDescription ifAbsentPut: [ MetacelloMVSProjectVersion new ]
]

{ #category : 'accessing' }
MetacelloMVSGraph >> givenProjectVersionOf: aRepository at: aVersion [
	"The project version given by hand for aRepository at the version or ref named aVersion, for whatever project asks it there"

	^ self givenProjectVersionOf: aRepository , ':' , aVersion
]

{ #category : 'project specs' }
MetacelloMVSGraph >> hasVersions: aProjectSpec ifFail: aBlock [
	"Whether aProjectSpec names one repository, and that repository has versions (see versionsOf:ifFail:). When the repository cannot be created or its versions cannot be read, aBlock is given why"

	| repository |
	aProjectSpec repositoryDescriptions size = 1 ifFalse: [ ^ false ].
	repository := [ self repositoryUrlOf: aProjectSpec ]
		              on: Error
		              do: [ :error | ^ aBlock value: (self descriptionOf: aProjectSpec) , ': ' , error description ].
	^ (self versionsOf: repository ifFail: aBlock) isNotEmpty
]

{ #category : 'reading' }
MetacelloMVSGraph >> initializeOn: aVersionSource [
	"Facts the graph was not given by hand are asked to aVersionSource, once"

	super initialize.
	source := aVersionSource.
	versions := Dictionary new.
	dates := Dictionary new.
	projectVersions := Dictionary new.
	givenProjectVersions := Dictionary new.
	repositories := Dictionary new
]

{ #category : 'project specs' }
MetacelloMVSGraph >> projectOf: aProjectSpec [
	"The name of its project: its baseline's (an alias made with project:copyFrom:with: names the same baseline)"
	^ aProjectSpec className allButFirst: 'BaselineOf' size
]

{ #category : 'reading' }
MetacelloMVSGraph >> projectVersionFrom: aBaselineDefinitions of: aCandidate [
	"Run the baseline in the sandbox, as the version of aCandidate. Never signals: the baseline is third-party code, so whatever it fails with becomes a problem saying the cause; the report names the version. A baseline that does not compile says why in one line. The problems are about the request of aCandidate, the first that asked for this version"

	| projectVersion |
	projectVersion := MetacelloMVSProjectVersion new
		           request: aCandidate request;
		           yourself.
	aBaselineDefinitions problems do: [ :each |
			projectVersion add: (MetacelloResolutionFinding
					 severity: each second
					 kind: each first
					 text: each third
					 request: aCandidate request) ].
	aBaselineDefinitions hasBaseline ifFalse: [ ^ projectVersion ].
	[
		[ self read: aBaselineDefinitions of: aCandidate into: projectVersion ]
			on: OCCompilationError
			do: [ :error | projectVersion error: #baselineFailure text: (self causeOf: error) ] ]
		on: Error
		do: [ :error | projectVersion error: #baselineFailure text: error description ].
	^ projectVersion
]

{ #category : 'reading' }
MetacelloMVSGraph >> projectVersionOf: aCandidate [
	"The project of aCandidate at its version, with what its baseline declares: given by hand for its repository at that version, or read once from the definitions the source answers. Kept by where it is read from and by project: one repository can hold the baselines of several projects (Spec2 and SpecCore)"

	^ projectVersions at: aCandidate repositoryDescription -> aCandidate project ifAbsentPut: [
		  givenProjectVersions
			  at: aCandidate repositoryDescription
			  ifAbsent: [ self projectVersionFrom: (source baselineOf: aCandidate) of: aCandidate ] ]
]

{ #category : 'reading' }
MetacelloMVSGraph >> read: aBaselineDefinitions of: aCandidate into: aProjectVersion [

	| sandbox |
	sandbox := MetacelloMVSBaselineSandbox
		           onDefinitions: aBaselineDefinitions definitions
		           repository: aBaselineDefinitions repositoryDescription.
	aProjectVersion repositoryDescription: aBaselineDefinitions repositoryDescription.
	self checkPlatformOf: aCandidate in: sandbox into: aProjectVersion.
	self warnAboutBaselineOf: aCandidate in: sandbox into: aProjectVersion.
	aProjectVersion replacements: sandbox replacements.
	aProjectVersion references: (sandbox requirementsInto: aProjectVersion)
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository at: aVersion projectVersion: aProjectVersion [
	"Given by hand: the project version of aRepository at the version or ref named aVersion, for whatever project asks it there"

	givenProjectVersions at: aRepository , ':' , aVersion put: aProjectVersion
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository at: aVersion replaces: aDictionary [
	"Given by hand: the baseline of aRepository at aVersion replaces project name -> repository string"

	(self givenProjectVersionOf: aRepository at: aVersion) replacements: aDictionary
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository at: aVersion requires: aCollectionOfProjectSpecs [
	"Given by hand: the baseline of aRepository at aVersion requires aCollectionOfProjectSpecs, and says nothing else"

	(self givenProjectVersionOf: aRepository at: aVersion) references: aCollectionOfProjectSpecs
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository dates: aDictionary [
	"Version -> DateAndTime of what each version points to"

	dates at: aRepository put: aDictionary
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository versions: aCollection [
	"Told, not asked: the source is not asked about aRepository, its dates included"

	versions at: aRepository put: aCollection asArray.
	dates at: aRepository ifAbsentPut: [ Dictionary new ]
]

{ #category : 'project specs' }
MetacelloMVSGraph >> repositoryFor: aProjectSpec [
	"The repository Metacello's platform creates for the one repository aProjectSpec names (Iceberg's when it integrates, Monticello's otherwise). Once per description"

	^ repositories
		  at: (self descriptionOf: aProjectSpec)
		  ifAbsentPut: [ MetacelloPlatform current createRepository: aProjectSpec repositorySpecs first ]
]

{ #category : 'project specs' }
MetacelloMVSGraph >> repositoryUrlOf: aProjectSpec [
	"Where the project aProjectSpec asks for lives, without the version it names: type://owner/name for git, the whole description for a directory (see MCRepository>>projectUrl). What the graph knows is kept per repository"

	^ (self repositoryFor: aProjectSpec) projectUrl
]

{ #category : 'project specs' }
MetacelloMVSGraph >> specFor: aRepositoryString project: aProjectName loads: aCollectionOfNames [
	"A project spec asking aRepositoryString (repository, version, path) for the project aProjectName, loading aCollectionOfNames (empty for its default groups)"

	^ (MetacelloProjectSpec project: MetacelloProject new)
		  repository: aRepositoryString;
		  name: aProjectName;
		  className: 'BaselineOf' , aProjectName;
		  loads: aCollectionOfNames;
		  yourself
]

{ #category : 'project specs' }
MetacelloMVSGraph >> versionStringOf: aProjectSpec [
	"The version aProjectSpec names after the repository, as written, or nil"

	^ (self repositoryFor: aProjectSpec) projectVersion ifNotNil: [ :string | string ifEmpty: [ nil ] ]
]

{ #category : 'versions' }
MetacelloMVSGraph >> versionsOf: aRepository ifFail: aBlock [
	"The version names of aRepository, read once from the source. When they cannot be read, aBlock is given why, at that moment"
	| published |
	versions at: aRepository ifPresent: [ :names | ^ names ].
	published := source versionsOf: aRepository ifFail: [ :text | ^ aBlock value: text ].
	dates at: aRepository put: ((published select: [ :each | each date notNil ]) collect: [ :each | each name -> each date ]) asDictionary.
	^ versions at: aRepository put: (published collect: [ :each | each name ]) asArray
]

{ #category : 'reading' }
MetacelloMVSGraph >> warnAboutBaselineOf: aCandidate in: aSandbox into: aProjectVersion [
	"Each text says what its kind does not already say (SystemVersion). The finding is about aCandidate, so the report names it"

	aSandbox imageQueries ifNotEmpty: [ :queries | aProjectVersion warning: #imageQuery text: (', ' join: queries) ].
	aSandbox usesRemovedMetacelloAPI ifTrue: [
		aProjectVersion warning: #removedAPI text: 'packageRepositoryURL, replaced by packageRepositoryURLForSpec:' ].
	aSandbox copiesProjects ifTrue: [ aProjectVersion warning: #copiedProject text: 'project:copyFrom:with:' ]
]
