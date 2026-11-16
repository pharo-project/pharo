"
I am a version source that answers from versions and baselines given up front: replayed from what another source answered (see MetacelloMVSRecordingSource), or written by hand in tests (repository:version:project:baseline:). I keep versions, dates, failures to read versions, and the definitions of each baseline. storeOn: writes me as Smalltalk code, to keep as a fixture.
"
Class {
	#name : 'MetacelloMVSRecordedSource',
	#superclass : 'MetacelloMVSVersionSource',
	#instVars : [
		'dates',
		'baselines',
		'versions',
		'versionFailures',
		'redirects',
		'unreadable'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'building' }
MetacelloMVSRecordedSource >> addAll: aRecordedSource [
	"Add what aRecordedSource recorded, so that several recordings answer as one"

	self recordings with: aRecordedSource recordings do: [ :mine :theirs | mine addAll: theirs ]
]

{ #category : 'versions' }
MetacelloMVSRecordedSource >> baselineOf: aCandidate [
	"What the baseline of the project of aCandidate was when it was recorded, where it is read from (its repository at its version, or the description of its dev root); what made its repository unreadable there; or that it was not recorded"

	^ baselines at: aCandidate repositoryDescription -> ('BaselineOf' , aCandidate project) ifAbsent: [
			  unreadable
				  at: aCandidate repositoryDescription
				  ifAbsent: [
				  MetacelloMVSBaselineDefinitions error: #repositoryFailure text: 'Not recorded: the baseline of ' , aCandidate printString ] ]
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> initialize [

	super initialize.
	versions := Dictionary new.
	versionFailures := Dictionary new.
	dates := Dictionary new.
	baselines := Dictionary new.
	unreadable := Dictionary new.
	redirects := Dictionary new
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> problemsFrom: aCollectionOfTriples into: aBaselineDefinitions [

	aBaselineDefinitions problems addAll: aCollectionOfTriples.
	^ aBaselineDefinitions
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> readFrom: aRepositoryDescription baseline: aBaselineDefinitions [
	"A baseline read from aRepositoryDescription (a repository at a version or ref), kept by where it was read from and by its class: one repository can hold the baselines of several projects. One that could not be read (no baseline class known) is kept for where it was read from"

	aBaselineDefinitions hasBaseline
		ifTrue: [
				baselines
					at: aRepositoryDescription -> (aBaselineDefinitions definitions detect: [ :each | each isClassDefinition ]) className asString
					put: aBaselineDefinitions ]
		ifFalse: [ unreadable at: aRepositoryDescription put: aBaselineDefinitions ]
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> recordings [

	^ {
		  versions.
		  versionFailures.
		  dates.
		  baselines.
		  unreadable.
		  redirects }
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> redirect: aWrittenDescription to: aDescription [
	"The repository written aWrittenDescription (github://olekscode/SingularizePluralize:v1.0/src) answered as aDescription (github://pharo-contributions/SingularizePluralize:v1.0/src): it was moved"

	redirects at: aWrittenDescription put: aDescription
]

{ #category : 'accessing' }
MetacelloMVSRecordedSource >> redirects [
	"Written repository description -> the description its repository answered, for the repositories that were moved"

	^ redirects
]

{ #category : 'accessing' }
MetacelloMVSRecordedSource >> replayPlatform [
	"A platform that makes repositories as the recording saw them: moved ones at their new place"

	^ MetacelloMVSReplayPlatform redirects: redirects
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> repository: aRepository at: aVersion baseline: aBaselineDefinitions [
	"A baseline read from aRepository at aVersion (see readFrom:baseline:)"

	self readFrom: aRepository , ':' , aVersion baseline: aBaselineDefinitions
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> repository: aRepository at: aVersion class: aClassTuple methods: aCollectionOfMethodTuples readFrom: aRepositoryDescription problems: aCollectionOfTriples [
	"The baseline of aRepository at aVersion: its class as (name, superclass name, instance variables, class variables, class instance variables), its methods as (meta, protocol, source), where it was read from, and the problems of reading it as (violation class name, severity, text)"

	| definitions |
	definitions := OrderedCollection new.
	definitions add: ((MCClassDefinition named: aClassTuple first)
			 superclassName: aClassTuple second;
			 category: aClassTuple first;
			 instVarNames: aClassTuple third;
			 classVarNames: aClassTuple fourth;
			 classInstVarNames: aClassTuple fifth;
			 yourself).
	aCollectionOfMethodTuples do: [ :each |
			definitions add: (MCMethodDefinition
					 className: aClassTuple first
					 classIsMeta: each first
					 selector: (OCParser parseMethod: each third) selector
					 category: each second
					 timeStamp: ''
					 source: each third) ].
	self repository: aRepository at: aVersion baseline: (self
			 problemsFrom: aCollectionOfTriples
			 into: (MetacelloMVSBaselineDefinitions definitions: definitions repository: aRepositoryDescription))
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> repository: aRepository at: aVersion unreadable: aCollectionOfTriples [
	"aRepository at aVersion had no baseline to read, for the problems aCollectionOfTriples"

	self
		repository: aRepository
		at: aVersion
		baseline: (self problemsFrom: aCollectionOfTriples into: MetacelloMVSBaselineDefinitions none)
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> repository: aRepository dates: aDictionary [

	dates at: aRepository put: aDictionary
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> repository: aRepository failsWith: aString [
	"The versions of aRepository could not be read, for the reason aString"

	versionFailures at: aRepository put: aString
]

{ #category : 'recording' }
MetacelloMVSRecordedSource >> repository: aRepository version: aVersion project: aProjectName baseline: aMethodSource [
	"aVersion of aRepository, whose BaselineOf<aProjectName> has the baseline: method aMethodSource. For tests written by hand"

	versions at: aRepository put: ((versions at: aRepository ifAbsent: [ #(  ) ]) copyWith: aVersion).
	self repository: aRepository at: aVersion baseline: (MetacelloMVSBaselineDefinitions
			 baselineClassNamed: 'BaselineOf' , aProjectName
			 method: aMethodSource
			 repository: aRepository , ':' , aVersion)
]

{ #category : 'building' }
MetacelloMVSRecordedSource >> repository: aRepository versions: aCollection [

	versions at: aRepository put: aCollection asArray
]

{ #category : 'printing' }
MetacelloMVSRecordedSource >> storeBaseline: aBaselineDefinitions of: aRepository at: aVersion on: aStream [
	"The baseline class and its own methods: the other classes of the baseline package are not read (see MetacelloMVSBaselineSandbox>>baselineClass)"

	| problems class |
	problems := (aBaselineDefinitions problems collect: [ :each | each asArray ]) asArray.
	aStream
		crtab;
		nextPutAll: 'repository: ';
		print: aRepository;
		nextPutAll: ' at: ';
		print: aVersion.
	aBaselineDefinitions hasBaseline ifFalse: [
			aStream
				nextPutAll: ' unreadable: ';
				print: problems;
				nextPut: $;.
			^ self ].
	class := aBaselineDefinitions definitions detect: [ :each | each isClassDefinition ].
	aStream
		nextPutAll: ' class: ';
		print: {
				class className asString.
				class superclassName asString.
				class instVarNames asArray.
				class classVarNames asArray.
				class classInstVarNames asArray };
		nextPutAll: ' methods: ';
		print:
			((aBaselineDefinitions definitions select: [ :each | each isMethodDefinition and: [ each className = class className ] ])
				 collect: [ :each |
						 {
							 each classIsMeta.
							 each category asString.
							 each source asString } ]) asArray;
		nextPutAll: ' readFrom: ';
		print: aBaselineDefinitions repositoryDescription;
		nextPutAll: ' problems: ';
		print: problems;
		nextPut: $;
]

{ #category : 'printing' }
MetacelloMVSRecordedSource >> storeOn: aStream [
	"Smalltalk code that builds this source again, to keep as a fixture"

	aStream nextPutAll: '(MetacelloMVSRecordedSource new'.
	(versions keys sort: [ :a :b | a <= b ]) do: [ :repository |
			aStream
				crtab;
				nextPutAll: 'repository: ';
				print: repository;
				nextPutAll: ' versions: ';
				print: ((versions at: repository) sort: [ :a :b | a <= b ]);
				nextPut: $;.
			(dates at: repository ifAbsent: [ nil ]) ifNotNil: [ :each |
					aStream
						crtab;
						nextPutAll: 'repository: ';
						print: repository;
						nextPutAll: ' dates: {'.
					(each keys sort: [ :a :b | a <= b ]) do: [ :version |
							aStream
								print: version;
								nextPutAll: ' -> (DateAndTime fromString: ';
								print: (each at: version) printString;
								nextPutAll: '). ' ].
					aStream nextPutAll: '} asDictionary;' ] ].
	(versionFailures keys sort: [ :a :b | a <= b ]) do: [ :repository |
			aStream
				crtab;
				nextPutAll: 'repository: ';
				print: repository;
				nextPutAll: ' failsWith: ';
				print: (versionFailures at: repository);
				nextPut: $; ].
	(redirects keys sort: [ :a :b | a <= b ]) do: [ :written |
			aStream
				crtab;
				nextPutAll: 'redirect: ';
				print: written;
				nextPutAll: ' to: ';
				print: (redirects at: written);
				nextPut: $; ].
	"Baselines are kept by where they are read from, repository:version, and by class"
	(baselines keys sort: [ :a :b | a printString <= b printString ]) do: [ :key |
			self
				storeBaseline: (baselines at: key)
				of: (key key copyUpToLast: $:)
				at: (key key copyAfterLast: $:)
				on: aStream ].
	(unreadable keys sort: [ :a :b | a <= b ]) do: [ :key |
			self
				storeBaseline: (unreadable at: key)
				of: (key copyUpToLast: $:)
				at: (key copyAfterLast: $:)
				on: aStream ].
	aStream
		crtab;
		nextPutAll: 'yourself)'
]

{ #category : 'accessing' }
MetacelloMVSRecordedSource >> versionsOf: aRepository ifFail: aBlock [

	| names known |
	versionFailures at: aRepository ifPresent: [ :text | ^ aBlock value: text ].
	names := versions at: aRepository ifAbsent: [ ^ aBlock value: 'Not recorded: the versions of ' , aRepository ].
	known := dates at: aRepository ifAbsent: [ Dictionary new ].
	^ names collect: [ :each | MetacelloProjectVersion name: each date: (known at: each ifAbsent: [ nil ]) ]
]
