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
		'pharoMajor',
		'repositories',
		'versions',
		'versionFailures'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'reading' }
MetacelloMVSGraph >> checkPlatformOf: aCandidate in: aSandbox into: aReading [
	"A declared minimum Pharo newer than this image is an error before anything is loaded (answer false); for: #pharoN.x blocks are accepted, with a warning"

	aSandbox pharoMinimum ifNotNil: [ :minimum |
			minimum > self pharoMajor ifTrue: [
					aReading
						error: #platformMismatch
						text:
						aCandidate printString , ' needs Pharo ' , minimum printString , ' or newer, this is Pharo ' , self pharoMajor printString.
					^ false ] ].
	aSandbox platformBlocks ifNotEmpty: [ :platforms |
			aReading
				warning: #platformBlock
				text: 'The baseline of ' , aCandidate printString , ' has blocks for ' , (', ' join: platforms)
					, ': its requirements depend on the image that resolves them' ].
	^ true
]

{ #category : 'accessing' }
MetacelloMVSGraph >> datesOf: aRepository [
	"Version -> DateAndTime of what each version of aRepository points to; empty when not known"

	^ dates
		  at: aRepository
		  ifAbsent: [ source ifNil: [ Dictionary new ] ifNotNil: [ dates at: aRepository put: (source datesOf: aRepository) ] ]
]

{ #category : 'project specs' }
MetacelloMVSGraph >> descriptionOf: aProjectSpec [
	"The one repository description of aProjectSpec, as written (github://owner/name:ref/path)"

	^ aProjectSpec repositoryDescriptions first
]

{ #category : 'accessing' }
MetacelloMVSGraph >> failureOf: aRepository [
	"Why the versions of aRepository cannot be read, or nil"

	self versionsOf: aRepository.
	^ versionFailures at: aRepository ifAbsent: [ nil ]
]

{ #category : 'project specs' }
MetacelloMVSGraph >> hasVersions: aProjectSpec [
	"Whether aProjectSpec names one repository that has versions: one that answers a project path and a version"

	^ (self repositoryFor: aProjectSpec) isNotNil
]

{ #category : 'accessing' }
MetacelloMVSGraph >> initialize [

	super initialize.
	versions := Dictionary new.
	versionFailures := Dictionary new.
	dates := Dictionary new.
	readings := Dictionary new.
	repositories := Dictionary new
]

{ #category : 'project specs' }
MetacelloMVSGraph >> pathOf: aProjectSpec [
	"The path of its code inside its repository, or nil"

	^ (self repositoryFor: aProjectSpec) repoPath ifNotNil: [ :path | path ifEmpty: [ nil ] ]
]

{ #category : 'reading' }
MetacelloMVSGraph >> pharoMajor [
	"The major version of Pharo that baselines are checked against: this image's, unless told"

	^ pharoMajor ifNil: [ SystemVersion current major ]
]

{ #category : 'reading' }
MetacelloMVSGraph >> pharoMajor: anInteger [

	pharoMajor := anInteger
]

{ #category : 'project specs' }
MetacelloMVSGraph >> projectOf: aProjectSpec [
	"The name of its project: its baseline's (an alias made with project:copyFrom:with: names the same baseline), or nil when unknown"

	^ aProjectSpec className ifNil: [ aProjectSpec name ] ifNotNil: [ :className | className allButFirst: 'BaselineOf' size ]
]

{ #category : 'reading' }
MetacelloMVSGraph >> read: aBaselineDefinitions of: aCandidate into: aReading [

	| sandbox |
	sandbox := MetacelloMVSBaselineSandbox
		           onDefinitions: aBaselineDefinitions definitions
		           repository: aBaselineDefinitions repositoryDescription.
	aReading repositoryDescription: aBaselineDefinitions repositoryDescription.
	(self checkPlatformOf: aCandidate in: sandbox into: aReading) ifFalse: [ ^ self ].
	self warnAboutBaselineOf: aCandidate in: sandbox into: aReading.
	aReading replacements: sandbox replacements.
	aReading references: (sandbox requirementsInto: aReading)
]

{ #category : 'reading' }
MetacelloMVSGraph >> readingFrom: aBaselineDefinitions of: aCandidate [
	"Run the baseline in the sandbox, as the version of aCandidate. Never signals: the baseline is third-party code, so whatever it fails with becomes a problem naming the version. The problems are about the request of aCandidate, the first that asked for this version"

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
	[ self read: aBaselineDefinitions of: aCandidate into: reading ]
		on: Error
		do: [ :error |
				reading error: #baselineFailure text: 'The baseline of ' , aCandidate printString , ' fails: ' , error description
					,
					'. If this version is too old for this Pharo, it is a platform break, not a missed major: the root can replace: it with a newer version of '
					, aCandidate repository ].
	^ reading
]

{ #category : 'reading' }
MetacelloMVSGraph >> readingOf: aCandidate [
	"The reading of the baseline of the version of aCandidate: told, or read once from the definitions the source answers"

	^ readings at: aCandidate repository -> aCandidate version string ifAbsentPut: [
			  source
				  ifNil: [
						  MetacelloMVSBaselineReading new
							  request: aCandidate request;
							  error: #repositoryFailure text: 'No requirements known for ' , aCandidate printString;
							  yourself ]
				  ifNotNil: [ self readingFrom: (source baselineOf: aCandidate) of: aCandidate ] ]
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
			  repository := [ aProjectSpec repositorySpecs first createRepository ]
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

{ #category : 'accessing' }
MetacelloMVSGraph >> source: aVersionSource [
	"Facts the graph was not told are asked to aVersionSource, once"

	source := aVersionSource
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

{ #category : 'accessing' }
MetacelloMVSGraph >> versionsOf: aRepository [
	"The versions of aRepository; none when they cannot be read, and failureOf: says why"

	^ versions at: aRepository ifAbsent: [
			  versions at: aRepository put: (source
					   ifNil: [
							   versionFailures at: aRepository put: 'No versions known for ' , aRepository.
							   #(  ) ]
					   ifNotNil: [
							   (source versionsOf: aRepository ifFail: [ :text |
									    versionFailures at: aRepository put: text.
									    #(  ) ]) asArray ]) ]
]

{ #category : 'reading' }
MetacelloMVSGraph >> warnAboutBaselineOf: aCandidate in: aSandbox into: aReading [

	aSandbox imageQueries ifNotEmpty: [ :queries |
			aReading warning: #imageQuery text: 'The baseline of ' , aCandidate printString , ' asks the image (' , (', ' join: queries)
				, '): its requirements can depend on the image that resolves them' ].
	aSandbox usesRemovedMetacelloAPI ifTrue: [
			aReading warning: #removedAPI text: 'The baseline of ' , aCandidate printString
				, ' sends packageRepositoryURL, which Metacello replaced by packageRepositoryURLForSpec:' ].
	aSandbox copiesProjects ifTrue: [
			aReading
				warning: #copiedProject
				text:
				'The baseline of ' , aCandidate printString , ' uses project:copyFrom:with:, a second name for a project it already requires' ]
]
