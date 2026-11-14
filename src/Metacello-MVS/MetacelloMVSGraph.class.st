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
		'readings',
		'repositories',
		'versions'
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
	"A graph asks aVersionSource the facts it was not told"
	^ self basicNew initializeOn: aVersionSource
]

{ #category : 'reading' }
MetacelloMVSGraph >> causeOf: aCompilationError [
	"Why the baseline does not compile, in one line: what the compiler noticed"

	^ ', ' join: (aCompilationError compilationResult ast allNotices collect: [ :each | each messageText ])
]

{ #category : 'reading' }
MetacelloMVSGraph >> checkPlatformOf: aCandidate in: aSandbox into: aReading [
	"for: #pharoN.x blocks are accepted, with a warning: the requirements then depend on the image"

	aSandbox platformBlocks ifNotEmpty: [ :platforms | aReading warning: #platformBlock text: (', ' join: platforms) ]
]

{ #category : 'versions' }
MetacelloMVSGraph >> datesOf: aRepository [
	"Version name -> DateAndTime of each version of aRepository whose date is known. Asked once its versions are read (see versionsOf:ifFail:)"
	^ dates at: aRepository
]

{ #category : 'project specs' }
MetacelloMVSGraph >> descriptionOf: aProjectSpec [
	"The one repository description of aProjectSpec, as written (github://owner/name:ref/path)"

	^ aProjectSpec repositoryDescriptions first
]

{ #category : 'project specs' }
MetacelloMVSGraph >> hasVersions: aProjectSpec [
	"Whether aProjectSpec names one repository that has versions: one that answers a project path and a version"

	^ (self repositoryFor: aProjectSpec) isNotNil
]

{ #category : 'initialization' }
MetacelloMVSGraph >> initializeOn: aVersionSource [
	"Facts the graph was not told are asked to aVersionSource, once"
	super initialize.
	source := aVersionSource.
	versions := Dictionary new.
	dates := Dictionary new.
	readings := Dictionary new.
	repositories := Dictionary new.
]

{ #category : 'project specs' }
MetacelloMVSGraph >> pathOf: aProjectSpec [
	"The path of its code inside its repository, or nil"

	^ (self repositoryFor: aProjectSpec) repoPath ifNotNil: [ :path | path ifEmpty: [ nil ] ]
]

{ #category : 'project specs' }
MetacelloMVSGraph >> projectOf: aProjectSpec [
	"The name of its project: its baseline's (an alias made with project:copyFrom:with: names the same baseline)"
	^ aProjectSpec className allButFirst: 'BaselineOf' size
]

{ #category : 'reading' }
MetacelloMVSGraph >> read: aBaselineDefinitions of: aCandidate into: aReading [

	| sandbox |
	sandbox := MetacelloMVSBaselineSandbox
		           onDefinitions: aBaselineDefinitions definitions
		           repository: aBaselineDefinitions repositoryDescription.
	aReading repositoryDescription: aBaselineDefinitions repositoryDescription.
	self checkPlatformOf: aCandidate in: sandbox into: aReading.
	self warnAboutBaselineOf: aCandidate in: sandbox into: aReading.
	aReading replacements: sandbox replacements.
	aReading references: (sandbox requirementsInto: aReading)
]

{ #category : 'reading' }
MetacelloMVSGraph >> readingFrom: aBaselineDefinitions of: aCandidate [
	"Run the baseline in the sandbox, as the version of aCandidate. Never signals: the baseline is third-party code, so whatever it fails with becomes a problem saying the cause; the report names the version. A baseline that does not compile says why in one line. The problems are about the request of aCandidate, the first that asked for this version"

	| reading |
	reading := MetacelloMVSBaselineReading new
		           request: aCandidate request;
		           yourself.
	aBaselineDefinitions problems do: [ :each |
			reading add: (MetacelloResolutionFinding
					 severity: each second
					 kind: each first
					 text: each third
					 request: aCandidate request) ].
	aBaselineDefinitions hasBaseline ifFalse: [ ^ reading ].
	[
		[ self read: aBaselineDefinitions of: aCandidate into: reading ]
			on: OCCompilationError
			do: [ :error | reading error: #baselineFailure text: (self causeOf: error) ] ]
		on: Error
		do: [ :error | reading error: #baselineFailure text: error description ].
	^ reading
]

{ #category : 'reading' }
MetacelloMVSGraph >> readingOf: aCandidate [
	"The reading of the baseline of the version of aCandidate: told, or read once from the definitions the source answers"
	^ readings at: aCandidate repository -> aCandidate version string ifAbsentPut: [ self readingFrom: (source baselineOf: aCandidate) of: aCandidate ]
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository at: aVersion reading: aReading [
	"Told: the reading of the baseline of aRepository at aVersion"

	readings at: aRepository -> aVersion put: aReading
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository at: aVersion replaces: aDictionary [
	"Told: the baseline of aRepository at aVersion replaces project name -> repository string"

	(self toldReadingOf: aRepository at: aVersion) replacements: aDictionary
]

{ #category : 'accessing' }
MetacelloMVSGraph >> repository: aRepository at: aVersion requires: aCollectionOfProjectSpecs [
	"Told: the baseline of aRepository at aVersion requires aCollectionOfProjectSpecs, and says nothing else"

	(self toldReadingOf: aRepository at: aVersion) references: aCollectionOfProjectSpecs
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
	"The repository Metacello's platform creates for aProjectSpec (Iceberg's when it integrates, Monticello's otherwise), when it answers a project path, a version and a path inside it. nil otherwise. Once per description"

	| descriptions |
	descriptions := aProjectSpec repositoryDescriptions.
	descriptions size = 1 ifFalse: [ ^ nil ].
	^ repositories at: descriptions first ifAbsentPut: [
			  | repository |
			  repository := [ MetacelloPlatform current createRepository: aProjectSpec repositorySpecs first ]
				                on: Error
				                do: [ nil ].
			  (repository isNotNil and: [ (repository respondsTo: #projectPath) and: [ repository projectPath isNotNil ] ])
				  ifTrue: [ repository ]
				  ifFalse: [ nil ] ]
]

{ #category : 'project specs' }
MetacelloMVSGraph >> repositoryUrlOf: aProjectSpec [
	"The URL of the repository aProjectSpec asks for, without its version or path: type://owner/name. What the graph knows is kept per repository"

	^ aProjectSpec repositorySpecs first type , '://' , (self repositoryFor: aProjectSpec) projectPath
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

{ #category : 'accessing' }
MetacelloMVSGraph >> toldReadingOf: aRepository at: aVersion [

	^ readings at: aRepository -> aVersion ifAbsentPut: [ MetacelloMVSBaselineReading new ]
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
MetacelloMVSGraph >> warnAboutBaselineOf: aCandidate in: aSandbox into: aReading [
	"Each text says what its kind does not already say (SystemVersion). The finding is about aCandidate, so the report names it"

	aSandbox imageQueries ifNotEmpty: [ :queries | aReading warning: #imageQuery text: (', ' join: queries) ].
	aSandbox usesRemovedMetacelloAPI ifTrue: [
		aReading warning: #removedAPI text: 'packageRepositoryURL, replaced by packageRepositoryURLForSpec:' ].
	aSandbox copiesProjects ifTrue: [ aReading warning: #copiedProject text: 'project:copyFrom:with:' ]
]
