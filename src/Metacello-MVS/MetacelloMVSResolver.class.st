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
		'currentRequirement',
		'askedGroups',
		'defaultAsked',
		'requiredFrom',
		'locked',
		'assumedInstalled',
		'rootRequest',
		'resolution',
		'dropsUnused',
		'rootRepositories'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSResolver class >> on: aGraph [

	^ self new setGraph: aGraph
]

{ #category : 'resolving' }
MetacelloMVSResolver >> ask: aProjectSpec [
	"Add the groups aProjectSpec asks (its loads, or the default ones) to those of its repository. Answer whether they grew"

	| repository loads asked size |
	repository := graph repositoryUrlOf: aProjectSpec.
	loads := aProjectSpec loads.
	(loads isNil or: [ loads isEmpty ]) ifTrue: [
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
MetacelloMVSResolver >> candidateFor: aProjectSpec version: aVersion requiredBy: aCandidateOrNil [
	"A candidate chosen for the request being resolved"

	^ (MetacelloMVSCandidate
		   resolvedProject: (MetacelloResolvedProject projectSpec: aProjectSpec)
		   repository: (graph repositoryUrlOf: aProjectSpec)
		   path: (graph pathOf: aProjectSpec))
		  version: aVersion;
		  requiredBy: aCandidateOrNil;
		  request: (self requestAbout: nil);
		  yourself
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkChronologyOf: aRepository about: aCandidate [
	"Within one vX.Y, a higher patch is never older than a lower one: an inversion (a version moved or misplaced) is a warning. Across minors and majors no order holds, a v2.1.5 can follow a v3.0.0. Checked once per repository"

	| dates versions |
	(chronologyChecked includes: aRepository) ifTrue: [ ^ self ].
	chronologyChecked add: aRepository.
	dates := graph datesOf: aRepository.
	versions := ((graph versionsOf: aRepository) select: [ :each | (MetacelloSemanticVersionNumber isSemanticVersion: each) and: [ dates includesKey: each ] ]) collect: [
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
								about: aCandidate ] ] ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkLocked: aDescription minimum: aMinimum against: aLock about: aCandidate [
	"aLock, a version locked in the registry, settles what aDescription asks, with a warning; a minimum of another major is an error"

	aMinimum major = aLock version major ifFalse: [
			^ self
				  error: #majorConflict
				  text: aDescription , ' needs another major than ' , aLock printString , ', locked in the registry'
				  about: aCandidate ].
	self
		warning: #heldByLock
		text: aDescription , ' is settled by ' , aLock printString , ', locked in the registry'
		about: aCandidate
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkPin: aProjectSpec by: aCandidate [
	"A requirement of a repository whose selected version is locked in the registry: the lock settles it and is never changed. Answer whether aProjectSpec is settled by it"

	| lock |
	lock := selected at: (graph repositoryUrlOf: aProjectSpec) ifAbsent: [ ^ false ].
	lock isLocked ifFalse: [ ^ false ].
	(self isVersioned: aProjectSpec) ifFalse: [ ^ false ].
	self
		checkLocked: (graph descriptionOf: aProjectSpec)
		minimum: (self minimumOf: aProjectSpec)
		against: lock
		about: aCandidate.
	^ true
]

{ #category : 'resolving' }
MetacelloMVSResolver >> checkProjectOf: aCandidate [
	"One project comes from one repository: two forks of it cannot both be loaded. Answer whether aCandidate may be selected"

	aCandidate project ifNil: [ ^ true ].
	projects
		at: aCandidate project
		ifPresent: [ :other |
				other repository = aCandidate repository ifFalse: [
						self
							error: #projectConflict
							text:
								'The project ' , aCandidate project , ' is required from two repositories: ' , aCandidate repository , ', and '
								, other repository , ' by:' , String lf , other chainString
							about: aCandidate.
						^ false ] ]
		ifAbsent: [ projects at: aCandidate project put: aCandidate ].
	^ true
]

{ #category : 'resolving' }
MetacelloMVSResolver >> consider: aCandidate [
	"An installed version is a requirement too, so that mvs never downgrades it. An installed version the legacy rule assumes is said"

	(selected includesKey: aCandidate repository) ifFalse: [
			installed at: aCandidate repository ifPresent: [ :each |
					(self installedCandidateFrom: each for: aCandidate) ifNotNil: [ :installedCandidate |
							(assumedInstalled notNil and: [ assumedInstalled includes: aCandidate repository ]) ifTrue: [
									self
										warning: #assumedInstalled
										text: installedCandidate printString
											,
											' is assumed installed: its packages are in the image, but it is not registered as loaded. Reload it to register it exactly'
										about: installedCandidate ].
							self select: installedCandidate ] ] ].
	self select: aCandidate
]

{ #category : 'initialization' }
MetacelloMVSResolver >> dropUnused: aBoolean [
	"Whether the resolution ends by dropping the projects that only versions not selected asked for (true by default). Go keeps them"

	dropsUnused := aBoolean
]

{ #category : 'findings' }
MetacelloMVSResolver >> error: aKind text: aString about: aCandidate [
	"Record an error of aKind about aCandidate (and the requirement being resolved). Answer nil: the caller stops there"

	self
		record: #error
		kind: aKind
		text: aString
		about: aCandidate.
	^ nil
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

{ #category : 'findings' }
MetacelloMVSResolver >> holdInRequirement: aFinding [

	currentRequirement ifNotNil: [ :requirement | requirement findings add: aFinding ]
]

{ #category : 'metacello' }
MetacelloMVSResolver >> installed: aDictionary [
	"Repository -> what the image has loaded from it: a project spec, or just its version"

	installed := aDictionary
]

{ #category : 'resolving' }
MetacelloMVSResolver >> installedCandidateFrom: aProjectSpecOrVersion for: aCandidate [
	"The candidate for the version the image has of the repository of aCandidate (locked or not); nil when the registration names no version"

	| spec ref |
	spec := aProjectSpecOrVersion isString
		        ifTrue: [ graph specFor: aCandidate repository , ':' , aProjectSpecOrVersion project: aCandidate project loads: #(  ) ]
		        ifFalse: [ aProjectSpecOrVersion ].
	ref := graph versionStringOf: spec.
	(ref notNil and: [ MetacelloSemanticVersionNumber isSemanticVersion: ref ]) ifFalse: [ ^ nil ].
	^ (self candidateFor: spec version: (MetacelloSemanticVersionNumber fromVersionString: ref) requiredBy: nil)
		  beInstalled;
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
MetacelloMVSResolver >> isLocked: aRepository [

	^ locked notNil and: [ locked includes: aRepository ]
]

{ #category : 'metacello' }
MetacelloMVSResolver >> isRequestOfASelectedVersion: aRequirement in: aResolution [
	"Only the requirements of the versions selected ask groups: superseded versions are not loaded"

	aRequirement reference ifNil: [ ^ false ].
	^ (aResolution buildList at: aRequirement from repository ifAbsent: [ nil ]) == aRequirement from
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

	| finding |
	finding := (resolution
		            findingKind: aFinding kind
		            severity: aFinding severity
		            text: aFinding text
		            about: aCandidate) ifNil: [ resolution add: aFinding about: aCandidate ].
	self holdInRequirement: finding
]

{ #category : 'resolving' }
MetacelloMVSResolver >> locked: aCollectionOfRepositories [
	"The repositories whose installed version the registry locks"

	locked := aCollectionOfRepositories asSet
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
MetacelloMVSResolver >> record: aSeverity kind: aKind text: aString about: aCandidate [
	"Keep a problem once, about the request it concerns (see requestAbout:)"

	| finding |
	finding := (resolution
		            findingKind: aKind
		            severity: aSeverity
		            text: aString
		            about: aCandidate) ifNil: [
			           resolution
				           add: (MetacelloResolutionFinding
						            severity: aSeverity
						            kind: aKind
						            text: aString
						            request: (self requestAbout: aCandidate))
				           about: aCandidate ].
	self holdInRequirement: finding
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
						self warning: #replaced text: name , ' is replaced by ' , repositoryString about: aRootCandidate ]
				ifFalse: [
						self
							error: #invalidReference
							text: name , ' is replaced by ' , repositoryString printString , ', not a with versions repository'
							about: aRootCandidate ] ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> replacementFor: aProjectSpec by: aCandidate [
	"The replacement of the project of aProjectSpec, if the root replaces it; a higher minimum than the replacement is ignored, with a warning"

	| replacement |
	replacement := replacements at: ((graph projectOf: aProjectSpec) ifNil: [ ^ aProjectSpec ]) ifAbsent: [ ^ aProjectSpec ].
	(graph descriptionOf: aProjectSpec) = (graph descriptionOf: replacement) ifTrue: [ ^ aProjectSpec ].
	((self isVersioned: aProjectSpec) and: [
		 (self isVersioned: replacement) and: [ (self minimumOf: replacement) < (self minimumOf: aProjectSpec) ] ]) ifTrue: [
			self
				warning: #higherThanReplace
				text: (graph descriptionOf: aProjectSpec) , ' asks for more than its replacement ' , (graph descriptionOf: replacement)
					, ', which is kept'
				about: aCandidate ].
	^ replacement
]

{ #category : 'findings' }
MetacelloMVSResolver >> requestAbout: aCandidateOrNil [
	"The request a finding about aCandidateOrNil is about: the current requirement's, else the one aCandidateOrNil was chosen for, else the root's"

	currentRequirement ifNotNil: [ :requirement | ^ requirement request ].
	^ (aCandidateOrNil ifNotNil: [ aCandidateOrNil request ]) ifNil: [ rootRequest ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> require: aRequiredSpec by: aCandidate [
	"Resolve one requirement, recorded as an MetacelloMVSRequirement (but the root's own). An error stops it there"

	aCandidate ifNotNil: [
		currentRequirement := resolution addRequirement: (MetacelloMVSRequirement from: aCandidate asked: aRequiredSpec) ].
	[
		| spec repository version grew |
		(graph hasVersions: aRequiredSpec) ifFalse: [
			^ self error: #invalidReference text: (self whyNoVersions: aRequiredSpec) about: aCandidate ].
		spec := self spell: (self replacementFor: aRequiredSpec by: aCandidate) by: aCandidate.
		repository := graph repositoryUrlOf: spec.
		currentRequirement ifNotNil: [ :requirement |
				requirement
					reference: spec;
					repository: repository ].
		grew := self ask: spec.
		(self checkPin: spec by: aCandidate) ifTrue: [
				grew ifTrue: [ self revisit: repository ].
				^ self ].
		(self isVersioned: spec) ifFalse: [
			^ self error: #unversionedReference text: (graph descriptionOf: spec) , ' names no version' about: aCandidate ].
		(self isFloating: spec)
			ifTrue: [
					self
						warning: #floatingVersion
						text: (graph descriptionOf: spec) , ' names a floating version, taken as its first version ' , (self minimumOf: spec) string
						about: aCandidate ]
			ifFalse: [
					(MetacelloSemanticVersionNumber isCanonical: (graph versionStringOf: spec)) ifFalse: [
						self
							warning: #nonCanonicalVersion
							text: (graph descriptionOf: spec) , ' names a version that is not vX.Y.Z'
							about: aCandidate ] ].
		currentRequirement ifNotNil: [ :requirement | requirement minimum: (self minimumOf: spec) ].
		version := self versionFor: spec requiredBy: aCandidate.
		version ifNil: [ ^ self ].
		(version isCanonical or: [ version string = (graph versionStringOf: spec) ]) ifFalse: [
				self
					warning: #nonCanonicalVersion
					text: repository , ' ' , version string , ' is selected, a version that is not vX.Y.Z'
					about: aCandidate ].
		self consider: (self candidateFor: spec version: version requiredBy: aCandidate).
		grew ifTrue: [ self revisit: repository ] ] ensure: [ currentRequirement := nil ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> reset [
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
	currentRequirement := nil.
	rootRequest := nil.
	resolution := MetacelloMVSResolution new
]

{ #category : 'metacello' }
MetacelloMVSResolver >> resolutionOf: aResolution [
	"aResolution, the walk done, for Metacello to record: when it has no errors, every selected project, the roots first in their order, with the requests of selected versions (each root's own first)"

	| roots |
	aResolution hasErrors ifTrue: [ ^ aResolution ].
	roots := aResolution rootCandidates.
	roots do: [ :candidate |
			(self resolvedProjectOf: candidate into: aResolution) ifNotNil: [ :project |
					aResolution addRoot: project.
					project addRequest: candidate request ] ].
	aResolution buildList valuesDo: [ :candidate |
			(roots includes: candidate) ifFalse: [
				(self resolvedProjectOf: candidate into: aResolution) ifNotNil: [ :project | aResolution add: project ] ] ].
	aResolution requirements do: [ :requirement |
			(self isRequestOfASelectedVersion: requirement in: aResolution) ifTrue: [
					(aResolution buildList at: requirement repository ifAbsent: [ nil ]) ifNotNil: [ :candidate |
						candidate resolvedProject addRequest: requirement request ] ] ].
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
	"Walk the requirements from each root aCollectionOfProjectSpecs names, in order, as far as possible and answer the MetacelloMVSResolution: the build list, the requirements met and the findings. Each project is read for the union of the groups its dependents ask. At the end, the projects nobody selected uses are dropped (see dropUnused:)"

	self reset.
	aCollectionOfProjectSpecs ifEmpty: [
			self error: #invalidReference text: 'Nothing to load: no root' about: nil.
			^ resolution ].
	rootRequest := MetacelloProjectRequest requester: nil projectSpec: aCollectionOfProjectSpecs first.
	aCollectionOfProjectSpecs do: [ :root |
			(graph hasVersions: root)
				ifTrue: [
				(rootRepositories includes: (graph repositoryUrlOf: root)) ifFalse: [ rootRepositories add: (graph repositoryUrlOf: root) ] ]
				ifFalse: [
				self error: #invalidReference text: (graph descriptionOf: root) printString , ' is not a repository with versions' about: nil ] ].
	resolution hasErrors ifTrue: [ ^ resolution ].
	resolution rootRepositories: rootRepositories.
	aCollectionOfProjectSpecs do: [ :spec | self require: spec by: nil ].
	[ pending isEmpty ] whileFalse: [ self visit: pending removeFirst ].
	resolution buildList: (dropsUnused
			 ifTrue: [ self usedIn: selected ]
			 ifFalse: [ selected ]).
	^ resolution
]

{ #category : 'metacello' }
MetacelloMVSResolver >> resolveRoots: someProjectSpecs engine: anEngine [
	"As a Metacello resolver: resolve the roots someProjectSpecs name, in order, each loading what its spec loads, counting what the registry says is installed (and locked), and answer a MetacelloMVSResolution"

	| roots |
	self installedFromRegistry.
	roots := someProjectSpecs collect: [ :each |
			         | descriptions |
			         descriptions := each repositoryDescriptions.
			         descriptions size = 1 ifFalse: [
					         self reset.
					         self
						         error: #invalidReference
						         text: 'mvs needs the one repository of ' , each name , ', not ' , descriptions size printString
						         about: nil.
					         ^ self resolutionOf: resolution ].
			         graph specFor: descriptions first project: each name loads: each loads ].
	^ self resolutionOf: (self resolveRoots: roots)
]

{ #category : 'metacello' }
MetacelloMVSResolver >> resolvedProjectOf: aCandidate into: aResolution [
	"The resolved project of aCandidate, given its spec at its version, its project built from the baseline read in the sandbox, so that Metacello records it without loading its BaselineOf. Its BaselineOf is installed after the load, unless the image has it from there or it is pinned"

	| reading project spec resolved |
	reading := graph readingOf: aCandidate.
	reading baselineClass ifNil: [
			aResolution addFinding: (MetacelloResolutionFinding
					 error: #baselineFailure
					 text: 'No baseline was read for ' , aCandidate printString
					 request: aCandidate request).
			^ nil ].
	project := [ reading newProject ]
		           on: Error
		           do: [ :error |
				           aResolution addFinding: (MetacelloResolutionFinding
						            error: #baselineFailure
						            text: 'The baseline of ' , aCandidate printString , ' fails: ' , error description
						            request: aCandidate request).
				           ^ nil ].
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
MetacelloMVSResolver >> select: aCandidate [

	| current |
	(self checkProjectOf: aCandidate) ifFalse: [ ^ self ].
	current := selected at: aCandidate repository ifAbsent: [ nil ].
	current ifNotNil: [
			current isLocked ifTrue: [
					^ self
						  checkLocked: aCandidate printString
						  minimum: aCandidate version
						  against: current
						  about: aCandidate ].
			current version major = aCandidate version major ifFalse: [
					^ self
						  error: #majorConflict
						  text:
							  'Two majors of ' , aCandidate repository , ' are required: ' , aCandidate version string , ', and ' , current version string
							  , ' by:' , String lf , current chainString
						  about: aCandidate ] ].
	(current isNil or: [ current version < aCandidate version ]) ifTrue: [ selected at: aCandidate repository put: aCandidate ].
	(visited includes: aCandidate repository -> aCandidate version) ifTrue: [ ^ self ].
	visited add: aCandidate repository -> aCandidate version.
	pending add: aCandidate
]

{ #category : 'initialization' }
MetacelloMVSResolver >> setGraph: aGraph [

	graph := aGraph.
	installed := Dictionary new.
	dropsUnused := true
]

{ #category : 'resolving' }
MetacelloMVSResolver >> spell: aProjectSpec by: aCandidate [
	"Repositories are told apart ignoring case: the first spelling seen stands for them all, and another spelling is a warning"

	| repository first |
	repository := graph repositoryUrlOf: aProjectSpec.
	first := spellings at: repository asLowercase ifAbsentPut: [ repository ].
	first = repository ifTrue: [ ^ aProjectSpec ].
	self
		warning: #spelling
		text: (graph descriptionOf: aProjectSpec) , ' spells ' , first , ' with another capitalization'
		about: aCandidate.
	^ graph
		  specFor: first , ((graph descriptionOf: aProjectSpec) allButFirst: repository size)
		  project: (graph projectOf: aProjectSpec)
		  loads: aProjectSpec loads
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
MetacelloMVSResolver >> versionFor: aProjectSpec requiredBy: aCandidate [
	"The lowest version at least the minimum, in its major. A prerelease only when it is the minimum itself. Between two spellings of one version (v1.2 and v1.2.0), the canonical one. nil, with an error, when there is none"

	| repository minimum candidates lowest |
	repository := graph repositoryUrlOf: aProjectSpec.
	minimum := self minimumOf: aProjectSpec.
	candidates := ((graph versionsOf: repository) select: [ :each | MetacelloSemanticVersionNumber isSemanticVersion: each ]) collect: [ :each |
		              MetacelloSemanticVersionNumber fromVersionString: each ].
	(graph failureOf: repository) ifNotNil: [ :text | ^ self error: #repositoryFailure text: text about: aCandidate ].
	self checkChronologyOf: repository about: aCandidate.
	candidates := candidates select: [ :each |
		              each major = minimum major and: [ each >= minimum and: [ each isPrerelease not or: [ each = minimum ] ] ] ].
	candidates ifEmpty: [
			^ self
				  error: #missingVersion
				  text: (graph descriptionOf: aProjectSpec) , ' has no version at least ' , minimum string , ' in major ' , minimum major printString
				  about: aCandidate ].
	lowest := candidates detectMin: [ :each | each ].
	^ (candidates select: [ :each | each = lowest ]) detect: [ :each | each isCanonical ] ifNone: [ lowest ]
]

{ #category : 'resolving' }
MetacelloMVSResolver >> visit: aCandidate [
	"Read the baseline of the version of aCandidate for the groups asked of its repository: its problems become findings of the resolution, the root's replaces apply, and each project spec it requires is required, once per version"

	| reading found specs already |
	reading := graph readingOf: aCandidate.
	found := MetacelloMVSBaselineReading new request: aCandidate request; yourself.
	specs := reading
		         referencesForGroups: (askedGroups at: aCandidate repository ifAbsent: [ #(  ) ])
		         includingDefault: (defaultAsked includes: aCandidate repository)
		         into: found.
	reading findings , found findings do: [ :each | self keep: each about: aCandidate ].
	((rootRepositories includes: aCandidate repository) and: [ aCandidate requiredBy isNil ])
		ifTrue: [ self replace: reading replacements about: aCandidate ]
		ifFalse: [
				reading replacements ifNotEmpty: [ :replaces |
						self
							warning: #ignoredReplace
							text: 'The baseline of ' , aCandidate printString , ' replaces ' , (', ' join: replaces keys sorted)
								, ', ignored: only the replaces of the root apply'
							about: aCandidate ] ].
	already := requiredFrom at: aCandidate repository -> aCandidate version ifAbsentPut: [ Set new ].
	specs do: [ :each |
			| key |
			key := each repositoryDescriptions asArray -> each loads.
			(already includes: key) ifFalse: [
					already add: key.
					self require: each by: aCandidate ] ]
]

{ #category : 'findings' }
MetacelloMVSResolver >> warning: aKind text: aString about: aCandidate [
	"Record a warning of aKind about aCandidate (and the requirement being resolved): accepted, but to deprecate"

	self
		record: #warning
		kind: aKind
		text: aString
		about: aCandidate
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
