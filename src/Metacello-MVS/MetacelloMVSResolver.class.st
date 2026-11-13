"
I resolve with minimal version selection, over a MetacelloMVSGraph. In a baseline, a version is a minimum; for each repository I keep the highest minimum anyone asks, and two majors of one repository are an error. What the image has installed counts as a requirement, so nothing is downgraded; a version locked in the registry is never changed.

I walk the requirements from a root as far as I can. Each version is read for the union of the groups asked of its repository, and read again when that union grows. I never signal: what I find becomes findings of the MetacelloMVSResolution I answer, and an error skips the requirement it is about.

I am also a MetacelloResolver (Metacello new mvs installs me): resolve:loads:engine: reads what is installed and locked from the registry, resolves, and answers a MetacelloMVSResolution whose projects Metacello records and loads.
"
Class {
	#name : 'MetacelloMVSResolver',
	#superclass : 'MetacelloResolver',
	#instVars : [
		'graph',
		'installed',
		'selected',
		'visited',
		'pending',
		'projects',
		'spellings',
		'chronologyChecked',
		'replacements',
		'askedGroups',
		'defaultAsked',
		'requiredFrom',
		'locked',
		'assumedInstalled',
		'resolution',
		'dropsUnused',
		'rootRepositories',
		'user'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSResolver class >> on: aGraph [

	^ self new setGraph: aGraph
]

{ #category : 'metacello' }
MetacelloMVSResolver >> addProjectOf: aCandidate to: aResolution [
	"Add the resolved project of aCandidate to aResolution"
	aResolution add: (self resolvedProjectOf: aCandidate into: aResolution ifNone: [ ^ self ])
]

{ #category : 'metacello' }
MetacelloMVSResolver >> addRoot: aCandidate to: aResolution [
	"Add the resolved project of the root aCandidate to aResolution, with its own request first"
	| project |
	project := self resolvedProjectOf: aCandidate into: aResolution ifNone: [ ^ self ].
	aResolution addRoot: project.
	project addRequest: aCandidate request
]

{ #category : 'resolving' }
MetacelloMVSResolver >> ask: aProjectSpec [
	"Add the groups aProjectSpec asks (its loads, or the default ones) to those of its repository. Answer whether they grew"

	| repository loads asked size |
	repository := graph repositoryUrlOf: aProjectSpec.
	loads := aProjectSpec loads.
	loads isEmpty ifTrue: [
			(defaultAsked includes: repository) ifTrue: [ ^ false ].
			defaultAsked add: repository.
			^ true ].
	asked := askedGroups at: repository ifAbsentPut: [ Set new ].
	size := asked size.
	asked addAll: loads.
	^ asked size > size
]

{ #category : 'resolving' }
MetacelloMVSResolver >> assumedInstalled: aCollectionOfRepositories [
	"The repositories installed only by the legacy rule: their version is the registration's, unchecked"

	assumedInstalled := aCollectionOfRepositories asSet
]

{ #category : 'versions' }
MetacelloMVSResolver >> candidateFor: aProjectSpec version: aVersion requiredBy: aRequester in: aRequirement [
	"A candidate chosen for aRequirement: its request is the one of aRequirement"

	^ (MetacelloMVSCandidateVersion
		   resolvedProject: (MetacelloResolvedProject projectSpec: aProjectSpec)
		   repository: (graph repositoryUrlOf: aProjectSpec)
		   path: (graph pathOf: aProjectSpec))
		  version: aVersion;
		  requiredBy: aRequester;
		  request: aRequirement request;
		  yourself
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkChronologyOf: aRepository about: aCandidate in: aRequirement [
	"Within one vX.Y, a higher patch is never older than a lower one: an inversion (a version moved or misplaced) is a warning. Across minors and majors no order holds, a v2.1.5 can follow a v3.0.0. Checked once per repository"

	| dates versions |
	(chronologyChecked includes: aRepository) ifTrue: [ ^ self ].
	chronologyChecked add: aRepository.
	dates := graph datesOf: aRepository.
	versions := ((graph versionsOf: aRepository ifFail: [ ^ self ]) select: [ :each | (MetacelloSemanticVersionNumber isSemanticVersion: each) and: [ dates includesKey: each ] ]) collect: [
		            :each | MetacelloSemanticVersionNumber fromVersionString: each ].
	(versions groupedBy: [ :each | each major -> each minor ]) valuesDo: [ :series |
			| sorted |
			sorted := series asSortedCollection asArray.
			sorted overlappingPairsDo: [ :lower :higher |
					(lower < higher and: [ (dates at: lower string) > (dates at: higher string) ]) ifTrue: [
							self
								warning: #chronologyInversion
								text:
									aRepository , ' ' , lower string , ' (' , (dates at: lower string) asDate yyyymmdd , ') is newer than ' , higher string , ' ('
									, (dates at: higher string) asDate yyyymmdd , ')'
								about: aCandidate in: aRequirement ] ] ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkLocked: aDescription minimum: aMinimum against: aLock about: aCandidate in: aRequirement [
	"aLock, a version locked in the registry, settles what aDescription asks, with a warning; a minimum of another major is an error"

	aMinimum major = aLock version major ifFalse: [
			^ self
				  error: #majorConflict
				  text: aDescription , ' needs another major than ' , aLock printString , ', locked in the registry'
				  about: aCandidate in: aRequirement ].
	self
		warning: #heldByLock
		text: aDescription , ' is settled by ' , aLock printString , ', locked in the registry'
		about: aCandidate in: aRequirement
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkPin: aProjectSpec by: aCandidate in: aRequirement [
	"A requirement of a repository whose selected version is locked in the registry: the lock settles it and is never changed. Answer whether aProjectSpec is settled by it"

	| lock |
	lock := selected at: (graph repositoryUrlOf: aProjectSpec) ifAbsent: [ ^ false ].
	lock isLocked ifFalse: [ ^ false ].
	(self isVersioned: aProjectSpec) ifFalse: [ ^ false ].
	self
		checkLocked: (graph descriptionOf: aProjectSpec)
		minimum: (self minimumOf: aProjectSpec)
		against: lock
		about: aCandidate in: aRequirement.
	^ true
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkProjectOf: aCandidate in: aRequirement [
	"One project comes from one repository: two forks of it cannot both be loaded. Answer whether aCandidate may be selected"

	projects
		at: aCandidate project
		ifPresent: [ :other |
				other repository = aCandidate repository ifFalse: [
						self
							error: #projectConflict
							text:
								'The project ' , aCandidate project , ' is required from two repositories: ' , aCandidate repository , ', and '
								, other repository , ', required by ' , other chainLine
							about: aCandidate
							in: aRequirement.
						^ false ] ]
		ifAbsent: [ projects at: aCandidate project put: aCandidate ].
	^ true
]

{ #category : 'initialization' }
MetacelloMVSResolver >> dropUnused: aBoolean [
	"Whether the resolution ends by dropping the projects that only versions not selected asked for (true by default). Go keeps them"

	dropsUnused := aBoolean
]

{ #category : 'findings' }
MetacelloMVSResolver >> error: aKind text: aString about: aCandidate in: aRequirement [
	"Record an error of aKind about aCandidate, found resolving aRequirement"

	self
		record: #error
		kind: aKind
		text: aString
		about: aCandidate
		in: aRequirement
]

{ #category : 'findings' }
MetacelloMVSResolver >> error: aKind text: aString on: aSubject [
	"Record an error of aKind about aSubject, found outside any requirement"

	self
		record: #error
		kind: aKind
		text: aString
		on: aSubject
]

{ #category : 'metacello' }
MetacelloMVSResolver >> hasBaselineOf: aProjectSpec [
	"The image has the BaselineOf of aProjectSpec from the repository it names"

	(Smalltalk globals includesKey: aProjectSpec className asSymbol) ifFalse: [ ^ false ].
	^ MetacelloProjectRegistry
		  registrationNamed: aProjectSpec className
		  ifPresent: [ :registration | registration repositoryDescriptions includesAll: aProjectSpec repositoryDescriptions ]
		  ifAbsent: [ false ]
]

{ #category : 'metacello' }
MetacelloMVSResolver >> installed: aDictionary [
	"Repository -> the project spec the image loaded from it"
	installed := aDictionary
]

{ #category : 'resolving' }
MetacelloMVSResolver >> installedCandidateFrom: aProjectSpec for: aCandidate in: aRequirement [
	"The candidate for the version the image has of the repository of aCandidate (locked or not), required by the image. Asked only when it is installed at a version (#isInstalledAtAVersion:)"
	^ (self candidateFor: aProjectSpec version: (MetacelloSemanticVersionNumber fromVersionString: (graph versionStringOf: aProjectSpec)) requiredBy: MetacelloImageRequest new in: aRequirement)
		  in: [ :candidate | (self isLocked: candidate repository) ifTrue: [ candidate beLocked ] ];
		  yourself
]

{ #category : 'metacello' }
MetacelloMVSResolver >> installedFromRegistry [
	"What the registry says the image has: repository -> the project spec each installed project was loaded from, the repositories locked, and those installed only by the legacy rule"

	| found locks assumed |
	found := Dictionary new.
	locks := Set new.
	assumed := Set new.
	MetacelloProjectRegistry registry registrations do: [ :registration |
			registration isInstalled ifTrue: [
					registration repositoryDescriptions do: [ :description |
							| spec |
							spec := graph specFor: description project: registration projectName loads: #(  ).
							(graph hasVersions: spec) ifTrue: [
									| repository |
									repository := graph repositoryUrlOf: spec.
									found at: repository put: spec.
									registration locked ifTrue: [ locks add: repository ].
									registration isInstalledByLegacyRule ifTrue: [ assumed add: repository ] ] ] ] ].
	self installed: found.
	self locked: locks.
	self assumedInstalled: assumed
]

{ #category : 'versions' }
MetacelloMVSResolver >> isFloating: aProjectSpec [

	^ (graph versionStringOf: aProjectSpec) ifNil: [ false ] ifNotNil: [ :ref | MetacelloSemanticVersionNumber isSeries: ref ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> isInstalledAtAVersion: aProjectSpec [
	"Whether what the image has installed, loaded from aProjectSpec, is at a semantic version, which can be compared with the versions asked. A branch or a commit is not"
	^ (graph versionStringOf: aProjectSpec)
		  ifNil: [ false ]
		  ifNotNil: [ :ref | MetacelloSemanticVersionNumber isSemanticVersion: ref ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> isLocked: aRepository [

	^ locked includes: aRepository
]

{ #category : 'metacello' }
MetacelloMVSResolver >> isRequestOfASelectedVersion: aRequirement in: aResolution [
	"Only the requirements of the versions selected, and the user's, ask groups: superseded versions are not loaded"

	^ aRequirement from isSelectedIn: aResolution
]

{ #category : 'versions' }
MetacelloMVSResolver >> isVersioned: aProjectSpec [
	"Whether aProjectSpec names a minimum: a version string, or a floating version (vX.Y.x, vX.x.x) which moves with each release and counts as the first version of its series, to deprecate"

	| ref |
	ref := graph versionStringOf: aProjectSpec.
	ref ifNil: [ ^ false ].
	^ (MetacelloSemanticVersionNumber isSemanticVersion: ref) or: [ MetacelloSemanticVersionNumber isSeries: ref ]
]

{ #category : 'findings' }
MetacelloMVSResolver >> keep: aFinding about: aCandidate [
	"Keep aFinding, found reading the baseline of aCandidate, once"

	resolution
		findingKind: aFinding kind
		severity: aFinding severity
		text: aFinding text
		about: aCandidate
		ifNone: [ resolution add: aFinding about: aCandidate ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> locked: aCollectionOfRepositories [
	"The repositories whose installed version the registry locks"

	locked := aCollectionOfRepositories asSet
]

{ #category : 'resolving' }
MetacelloMVSResolver >> majorConflictOf: aCandidate against: anotherCandidate in: aRequirement [
	"An error, attached to aRequirement: aCandidate is another major than anotherCandidate, selected already. Its text names both versions; its description, who asked each"

	| text finding |
	text := aCandidate project , ' ' , anotherCandidate version string , ' and ' , aCandidate version string.
	finding := resolution
		           findingKind: #majorConflict
		           severity: #error
		           text: text
		           about: aCandidate
		           ifNone: [
				           resolution
					           addMajorConflict: (MetacelloResolutionFinding error: #majorConflict text: text request: aRequirement request)
					           about: aCandidate
					           against: anotherCandidate ].
	aRequirement findings add: finding
]

{ #category : 'versions' }
MetacelloMVSResolver >> minimumOf: aProjectSpec [
	"The lowest version aProjectSpec accepts"

	| ref |
	(self isVersioned: aProjectSpec) ifFalse: [ ^ self error: (graph descriptionOf: aProjectSpec) , ' names no version' ].
	ref := graph versionStringOf: aProjectSpec.
	(MetacelloSemanticVersionNumber isSeries: ref) ifTrue: [ ^ MetacelloSemanticVersionNumber fromSeries: ref ].
	^ MetacelloSemanticVersionNumber fromVersionString: ref
]

{ #category : 'findings' }
MetacelloMVSResolver >> record: aSeverity kind: aKind text: aString about: aCandidate in: aRequirement [
	"Keep a problem once, about the request of aRequirement, and attach it to aRequirement"

	| finding |
	finding := resolution
		           findingKind: aKind
		           severity: aSeverity
		           text: aString
		           about: aCandidate
		           ifNone: [
			           resolution
				           add: (MetacelloResolutionFinding
						            severity: aSeverity
						            kind: aKind
						            text: aString
						            request: aRequirement request)
				           about: aCandidate ].
	aRequirement findings add: finding
]

{ #category : 'findings' }
MetacelloMVSResolver >> record: aSeverity kind: aKind text: aString on: aSubject [
	"Keep a problem once, about the request of aSubject: found outside any requirement"

	resolution
		findingKind: aKind
		severity: aSeverity
		text: aString
		about: aSubject
		ifNone: [
				resolution
					add: (MetacelloResolutionFinding
							 severity: aSeverity
							 kind: aKind
							 text: aString
							 request: aSubject request)
					about: aSubject ]
]

{ #category : 'upgrades' }
MetacelloMVSResolver >> releasesOf: aRepository [
	"The versions of aRepository a baseline can ask as a minimum: no prereleases, tolerated spellings included. None when its versions cannot be read: that was reported already"

	^ (((graph versionsOf: aRepository ifFail: [ :why | ^ #(  ) ]) select: [ :each |
		    MetacelloSemanticVersionNumber isSemanticVersion: each ]) collect: [ :each |
		   MetacelloSemanticVersionNumber fromVersionString: each ]) reject: [ :each | each isPrerelease ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> replace: aDictionary about: aRootCandidate [
	"The replaces of the root apply, each with a warning: a replaced project is pinned to the exact version of its replacement"

	aDictionary keysAndValuesDo: [ :name :repositoryString |
			| spec |
			spec := graph specFor: repositoryString project: name loads: #(  ).
			(graph hasVersions: spec)
				ifTrue: [
						replacements at: name put: spec.
						self warning: #replaced text: name , ' is replaced by ' , repositoryString on: aRootCandidate ]
				ifFalse: [
						self
							error: #invalidReference
							text: name , ' is replaced by ' , repositoryString printString , ', not a with versions repository'
							on: aRootCandidate ] ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> replacementFor: aProjectSpec by: aCandidate in: aRequirement [
	"The replacement of the project of aProjectSpec, if the root replaces it; a higher minimum than the replacement is ignored, with a warning"

	| replacement |
	replacement := replacements at: (graph projectOf: aProjectSpec) ifAbsent: [ ^ aProjectSpec ].
	(graph descriptionOf: aProjectSpec) = (graph descriptionOf: replacement) ifTrue: [ ^ aProjectSpec ].
	((self isVersioned: aProjectSpec) and: [
		 (self isVersioned: replacement) and: [ (self minimumOf: replacement) < (self minimumOf: aProjectSpec) ] ]) ifTrue: [
			self
				warning: #higherThanReplace
				text: (graph descriptionOf: aProjectSpec) , ' asks for more than its replacement ' , (graph descriptionOf: replacement)
					, ', which is kept'
				about: aCandidate in: aRequirement ].
	^ replacement
]

{ #category : 'upgrades' }
MetacelloMVSResolver >> reportUpgrades [
	"The resolution is over: for each requirement of a selected version, report as infos the newer versions its baseline could ask for. Requirements of versions not selected are left out: a newer version of their baseline exists already"

	resolution requirements do: [ :each | (resolution isSuperseded: each) ifFalse: [ self reportUpgradesOf: each ] ]
]

{ #category : 'upgrades' }
MetacelloMVSResolver >> reportUpgradesOf: aRequirement [
	"Infos about what aRequirement could ask instead: the version MVS selected already (#raisedByOthers), the latest of its major (#newerInMajor), a newer major (#newerMajor), or, when it names no version, the latest version (#taggedVersionAvailable)"

	| releases asked latestInMajor newest |
	"A requirement that stopped before its reference was read (no versions) has no repository"
	aRequirement repository ifNil: [ ^ self ].
	releases := self releasesOf: aRequirement repository.
	releases ifEmpty: [ ^ self ].
	newest := releases max.
	asked := aRequirement from displayName , ' asks for ' , (graph descriptionOf: aRequirement asked).
	"A requirement that names no version, or is pinned, never gets a minimum"
	aRequirement minimum ifNil: [
			(aRequirement findings anySatisfy: [ :each | each kind = #unversionedReference ]) ifTrue: [
					self
						upgrade: #taggedVersionAvailable
						to: newest
						asked: 'no version'
						text: asked , ', which names no version; can ask for :' , newest string
						about: aRequirement ].
			^ self ].
	"A requirement whose version could not be found or selected is never resolved"
	aRequirement resolved ifNil: [ ^ self ].
	aRequirement isRaised ifTrue: [
			self
				upgrade: #raisedByOthers
				to: aRequirement resolved version
				asked: (graph versionStringOf: aRequirement reference)
				text:
				asked , '; already selected at ' , aRequirement resolved version string , ' by '
				, aRequirement resolved requiredBy displayName
				about: aRequirement ].
	latestInMajor := (releases select: [ :each | each major = aRequirement minimum major ]) max.
	latestInMajor > aRequirement resolved version ifTrue: [
			self
				upgrade: #newerInMajor
				to: latestInMajor
				asked: (graph versionStringOf: aRequirement reference)
				text: asked , '; can ask for :' , latestInMajor string
				about: aRequirement ].
	newest major > aRequirement minimum major ifTrue: [
			self
				upgrade: #newerMajor
				to: newest
				asked: (graph versionStringOf: aRequirement reference)
				text: asked , '; a newer major exists: ' , newest string
				about: aRequirement ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> require: aRequiredSpec by: aCandidate [
	"Resolve one requirement, recorded as a MetacelloMVSRequirement and passed to what it calls, which attach their findings to it. aCandidate asked for it: a candidate version, or the user for a root. An error stops it there"

	| requirement spec repository version grew |
	requirement := resolution addRequirement: (MetacelloMVSRequirement from: aCandidate asked: aRequiredSpec).
	(graph hasVersions: aRequiredSpec) ifFalse: [
		^ self error: #invalidReference text: (self whyNoVersions: aRequiredSpec) about: aCandidate in: requirement ].
	spec := self spell: (self replacementFor: aRequiredSpec by: aCandidate in: requirement) by: aCandidate in: requirement.
	repository := graph repositoryUrlOf: spec.
	requirement
		reference: spec;
		repository: repository.
	grew := self ask: spec.
	(self checkPin: spec by: aCandidate in: requirement) ifTrue: [
			grew ifTrue: [ self revisit: repository ].
			^ self ].
	(self isVersioned: spec) ifFalse: [
		^ self error: #unversionedReference text: (graph descriptionOf: spec) , ' names no version' about: aCandidate in: requirement ].
	(self isFloating: spec)
		ifTrue: [
				self
					warning: #floatingVersion
					text: (graph projectOf: spec) , ' ' , (graph versionStringOf: spec) , ', taken as ' , (self minimumOf: spec) string
					about: aCandidate
					in: requirement ]
		ifFalse: [
				(MetacelloSemanticVersionNumber isCanonical: (graph versionStringOf: spec)) ifFalse: [
					self
						warning: #nonCanonicalVersion
						text: (graph projectOf: spec) , ' ' , (graph versionStringOf: spec)
						about: aCandidate
						in: requirement ] ].
	requirement minimum: (self minimumOf: spec).
	version := self versionFor: spec requiredBy: aCandidate in: requirement ifNone: [ ^ self ].
	(version isCanonical or: [ version string = (graph versionStringOf: spec) ]) ifFalse: [
			self
				warning: #nonCanonicalVersion
				text: (graph projectOf: spec) , ' ' , version string , ', selected'
				about: aCandidate
				in: requirement ].
	self requireVersion: (self candidateFor: spec version: version requiredBy: aCandidate in: requirement) in: requirement.
	grew ifTrue: [ self revisit: repository ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> requireVersion: aCandidate in: aRequirement [
	"Each requirement can raise the selected version, never lower it. An installed version counts as a requirement, so that MVS never downgrades it. A version that is only assumed installed is reported with a warning."
	(selected includesKey: aCandidate repository) ifFalse: [
			installed at: aCandidate repository ifPresent: [ :each |
					(self isInstalledAtAVersion: each) ifTrue: [
							| installedCandidate |
							installedCandidate := self installedCandidateFrom: each for: aCandidate in: aRequirement.
							(assumedInstalled includes: aCandidate repository) ifTrue: [
									self
										warning: #assumedInstalled
										text: installedCandidate printString
											, ' is assumed installed: its packages are in the image, but it is not registered as loaded. Reload it to register it exactly'
										about: installedCandidate in: aRequirement ].
							self select: installedCandidate in: aRequirement ] ] ].
	self select: aCandidate in: aRequirement
]

{ #category : 'metacello' }
MetacelloMVSResolver >> resolutionOf: aResolution [
	"aResolution, the walk done, for Metacello to record: when it has no errors, every selected project, the roots first in their order, with the requests of selected versions (each root's own first)"
	| roots |
	aResolution hasErrors ifTrue: [ ^ aResolution ].
	roots := aResolution rootCandidates.
	roots do: [ :candidate | self addRoot: candidate to: aResolution ].
	aResolution buildList valuesDo: [ :candidate | (roots includes: candidate) ifFalse: [ self addProjectOf: candidate to: aResolution ] ].
	aResolution requirements do: [ :requirement |
			(self isRequestOfASelectedVersion: requirement in: aResolution) ifTrue: [
				(aResolution buildList at: requirement repository) resolvedProject addRequest: requirement request ] ].
	^ aResolution
]

{ #category : 'resolving' }
MetacelloMVSResolver >> resolve: aRepositoryString project: aProjectName [
	"As resolve:project:loads:, loading the default groups of the root"

	^ self resolve: aRepositoryString project: aProjectName loads: #(  )
]

{ #category : 'resolving' }
MetacelloMVSResolver >> resolve: aRepositoryString project: aProjectName loads: aCollectionOfGroups [
	"Resolve from one root, loading aCollectionOfGroups of it (empty for its default ones), and from the projects in dev mode, roots too (see resolveRoots:)"

	^ self resolveRoots: { (graph specFor: aRepositoryString project: aProjectName loads: aCollectionOfGroups) }
]

{ #category : 'resolving' }
MetacelloMVSResolver >> resolveRoots: aCollectionOfProjectSpecs [
	"Walk the requirements from each root aCollectionOfProjectSpecs names, in order, as far as possible and answer the MetacelloMVSResolution: the build list, the requirements met and the findings. The user requires the roots. Each project is read for the union of the groups its dependents ask. At the end, the projects nobody selected uses are dropped (see dropUnused:), and the newer versions the baselines could ask for are reported as infos (see reportUpgrades)"

	aCollectionOfProjectSpecs ifEmpty: [
			self error: #invalidReference text: 'Nothing to load: no root' on: user.
			^ resolution ].
	aCollectionOfProjectSpecs do: [ :root |
			(graph hasVersions: root)
				ifTrue: [
				(rootRepositories includes: (graph repositoryUrlOf: root)) ifFalse: [ rootRepositories add: (graph repositoryUrlOf: root) ] ]
				ifFalse: [ self error: #invalidReference text: (self whyNoVersions: root) on: user ] ].
	resolution hasErrors ifTrue: [ ^ resolution ].
	resolution rootRepositories: rootRepositories.
	aCollectionOfProjectSpecs do: [ :spec | self require: spec by: user ].
	[ pending isEmpty ] whileFalse: [ self visit: pending removeFirst ].
	resolution buildList: (dropsUnused
			 ifTrue: [ self usedIn: selected ]
			 ifFalse: [ selected ]).
	self reportUpgrades.
	^ resolution
]

{ #category : 'metacello' }
MetacelloMVSResolver >> resolveRoots: someProjectSpecs engine: anEngine [
	"As a Metacello resolver: resolve the roots someProjectSpecs name, in order, each loading what its spec loads, counting what the registry says is installed (and locked), and answer a MetacelloMVSResolution"
	self installedFromRegistry.
	^ self resolutionOf: (self resolveRoots: someProjectSpecs)
]

{ #category : 'metacello' }
MetacelloMVSResolver >> resolvedProjectOf: aCandidate into: aResolution ifNone: aBlock [
	"The resolved project of aCandidate, given its spec at its version, its project built from the baseline read in the sandbox, so that Metacello records it without loading its BaselineOf. Its BaselineOf is installed after the load, unless the image has it from there or it is pinned. When the baseline fails, the error is recorded and aBlock answers"

	| reading project spec resolved |
	reading := graph readingOf: aCandidate.
	project := [ reading newProject ]
		           on: Error
		           do: [ :error |
				           aResolution addFinding: (MetacelloResolutionFinding
						            error: #baselineFailure
						            text: aCandidate displayName , ': ' , error description
						            request: aCandidate request).
				           ^ aBlock value ].
	spec := (MetacelloProjectSpec project: project)
		        name: aCandidate project;
		        className: 'BaselineOf' , aCandidate project;
		        repository: reading repositoryDescription;
		        yourself.
	resolved := aCandidate resolvedProject.
	resolved changeVersionTo: spec.
	(aCandidate isLocked or: [ self hasBaselineOf: spec ]) ifFalse: [ resolved beInstallingBaseline ].
	^ resolved
]

{ #category : 'resolving' }
MetacelloMVSResolver >> revisit: aRepository [
	"The groups asked of aRepository grew: read its selected version again for them, unless it is still to be visited"

	| selection |
	selection := selected at: aRepository ifAbsent: [ ^ self ].
	((visited includes: aRepository -> selection version) and: [ (pending includes: selection) not ]) ifTrue: [
		pending add: selection ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> select: aCandidate in: aRequirement [

	(self checkProjectOf: aCandidate in: aRequirement) ifFalse: [ ^ self ].
	selected
		at: aCandidate repository
		ifPresent: [ :current |
				current isLocked ifTrue: [
						^ self
							  checkLocked: aCandidate printString
							  minimum: aCandidate version
							  against: current
							  about: aCandidate
							  in: aRequirement ].
				current version major = aCandidate version major ifFalse: [
					^ self majorConflictOf: aCandidate against: current in: aRequirement ].
				current version < aCandidate version ifTrue: [ selected at: aCandidate repository put: aCandidate ] ]
		ifAbsent: [ selected at: aCandidate repository put: aCandidate ].
	(visited includes: aCandidate repository -> aCandidate version) ifTrue: [ ^ self ].
	visited add: aCandidate repository -> aCandidate version.
	pending add: aCandidate
]

{ #category : 'initialization' }
MetacelloMVSResolver >> setGraph: aGraph [
	"A resolver resolves once: make another one to resolve again"

	graph := aGraph.
	installed := Dictionary new.
	locked := Set new.
	assumedInstalled := Set new.
	dropsUnused := true.
	selected := Dictionary new.
	projects := Dictionary new.
	spellings := Dictionary new.
	replacements := Dictionary new.
	chronologyChecked := Set new.
	visited := Set new.
	pending := OrderedCollection new.
	askedGroups := Dictionary new.
	defaultAsked := Set new.
	requiredFrom := Dictionary new.
	rootRepositories := OrderedCollection new.
	user := MetacelloUserRequest new.
	resolution := MetacelloMVSResolution new
]

{ #category : 'resolving' }
MetacelloMVSResolver >> spell: aProjectSpec by: aCandidate in: aRequirement [
	"Repositories are told apart ignoring case: the first spelling seen stands for them all, and another spelling is a warning"

	| repository first |
	repository := graph repositoryUrlOf: aProjectSpec.
	first := spellings at: repository asLowercase ifAbsentPut: [ repository ].
	first = repository ifTrue: [ ^ aProjectSpec ].
	self
		warning: #spelling
		text: (graph descriptionOf: aProjectSpec) , ' spells ' , first , ' with another capitalization'
		about: aCandidate in: aRequirement.
	^ graph
		  specFor: first , ((graph descriptionOf: aProjectSpec) allButFirst: repository size)
		  project: (graph projectOf: aProjectSpec)
		  loads: aProjectSpec loads
]

{ #category : 'upgrades' }
MetacelloMVSResolver >> upgrade: aKind to: aVersion asked: aString text: aText about: aRequirement [
	"Record once an info of aKind proposing aRequirement asks for aVersion instead of aString, and attach it to aRequirement"

	| finding |
	finding := resolution
		           findingKind: aKind
		           severity: #info
		           text: aText
		           about: aRequirement from
		           ifNone: [
				           resolution
					           add: ((MetacelloMVSUpgradeFinding info: aKind text: aText request: aRequirement request)
							            project: (graph projectOf: aRequirement asked) asked: aString version: aVersion string;
							            yourself)
					           about: aRequirement from ].
	aRequirement findings add: finding
]

{ #category : 'resolving' }
MetacelloMVSResolver >> usedIn: aBuildList [
	"The part of aBuildList reached from the roots through the selected versions only. A project only versions not selected asked for is unused. The versions kept are not changed"

	| used toVisit |
	used := Dictionary new.
	toVisit := OrderedCollection new.
	rootRepositories do: [ :each | (aBuildList includesKey: each) ifTrue: [ toVisit add: each ] ].
	[ toVisit isEmpty ] whileFalse: [
			| repository candidate |
			repository := toVisit removeFirst.
			candidate := aBuildList at: repository.
			(used includesKey: repository) ifFalse: [
					used at: repository put: candidate.
					resolution requirements do: [ :each |
						((each from isSameVersionAs: candidate) and: [ aBuildList includesKey: each repository ]) ifTrue: [
							toVisit add: each repository ] ] ] ].
	^ used
]

{ #category : 'resolving' }
MetacelloMVSResolver >> versionFor: aProjectSpec requiredBy: aCandidate in: aRequirement ifNone: aBlock [
	"The lowest version at least the minimum, in its major. A prerelease only when it is the minimum itself. Between two spellings of one version (v1.2 and v1.2.0), the canonical one. When there is none, the error is recorded and aBlock answers"

	| repository minimum candidates lowest |
	repository := graph repositoryUrlOf: aProjectSpec.
	minimum := self minimumOf: aProjectSpec.
	candidates := ((graph versionsOf: repository ifFail: [ :text |
			   self error: #repositoryFailure text: text about: aCandidate in: aRequirement.
			   ^ aBlock value ]) select: [ :each | MetacelloSemanticVersionNumber isSemanticVersion: each ]) collect: [ :each |
		              MetacelloSemanticVersionNumber fromVersionString: each ].
	self checkChronologyOf: repository about: aCandidate in: aRequirement.
	candidates := candidates select: [ :each |
		              each major = minimum major and: [ each >= minimum and: [ each isPrerelease not or: [ each = minimum ] ] ] ].
	candidates ifEmpty: [
			self
				error: #missingVersion
				text: (graph descriptionOf: aProjectSpec) , ' has no version at least ' , minimum string , ' in major ' , minimum major printString
				about: aCandidate in: aRequirement.
			^ aBlock value ].
	lowest := candidates detectMin: [ :each | each ].
	^ (candidates select: [ :each | each = lowest ]) detect: [ :each | each isCanonical ] ifNone: [ lowest ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> visit: aCandidate [
	"Read the baseline of the version of aCandidate for the groups asked of its repository: its problems become findings of the resolution, the root's replaces apply, and each project spec it requires is required, once per version"

	| reading found specs already |
	reading := graph readingOf: aCandidate.
	found := MetacelloMVSBaselineReading new
		         request: aCandidate request;
		         yourself.
	specs := reading
		         referencesForGroups: (askedGroups at: aCandidate repository ifAbsent: [ #(  ) ])
		         includingDefault: (defaultAsked includes: aCandidate repository)
		         into: found.
	reading findings , found findings do: [ :each | self keep: each about: aCandidate ].
	aCandidate mustReplace
		ifTrue: [ self replace: reading replacements about: aCandidate ]
		ifFalse: [
				reading replacements ifNotEmpty: [ :replaces |
						self
							warning: #ignoredReplace
							text: 'The baseline of ' , aCandidate printString , ' replaces ' , (', ' join: replaces keys sorted)
								, ', ignored: only the replaces of the root apply'
							on: aCandidate ] ].
	already := requiredFrom at: aCandidate repository -> aCandidate version ifAbsentPut: [ Set new ].
	specs do: [ :each |
			| key |
			key := each repositoryDescriptions asArray -> each loads.
			(already includes: key) ifFalse: [
					already add: key.
					self require: each by: aCandidate ] ]
]

{ #category : 'findings' }
MetacelloMVSResolver >> warning: aKind text: aString about: aCandidate in: aRequirement [
	"Record a warning of aKind about aCandidate, found resolving aRequirement: accepted, but to deprecate"

	self
		record: #warning
		kind: aKind
		text: aString
		about: aCandidate
		in: aRequirement
]

{ #category : 'findings' }
MetacelloMVSResolver >> warning: aKind text: aString on: aSubject [
	"Record a warning of aKind about aSubject, found outside any requirement: accepted, but to deprecate"

	self
		record: #warning
		kind: aKind
		text: aString
		on: aSubject
]

{ #category : 'resolving' }
MetacelloMVSResolver >> whyNoVersions: aProjectSpec [
	"Why aProjectSpec names no repository with versions: several repositories, or one that has no versions"

	| descriptions |
	descriptions := aProjectSpec repositoryDescriptions.
	descriptions size = 1 ifFalse: [
		^ (graph projectOf: aProjectSpec) printString , ' declares ' , descriptions size printString , ' repositories instead of one' ].
	^ descriptions first printString , ' is not a repository with versions'
]
