"
I am the resolution mvs answers: what the walk found, as far as it could go, and, when it resolves, the projects for Metacello to record (see MetacelloResolution).

buildList: repository -> the MetacelloMVSCandidateVersion chosen.
requirements: every MetacelloMVSRequirement met: what was asked, by which version, and what it resolved to.
rootRepository: the repository of the root.
rivals: major conflict -> the candidate version of the other major, so that the description tells who asked each.
subjects: finding -> the candidate it was found about, for its repository and its chain.

I resolve when I have no errors; with errors, the build list is incomplete and I hold no projects.
"
Class {
	#name : 'MetacelloMVSResolution',
	#superclass : 'MetacelloResolution',
	#instVars : [
		'buildList',
		'requirements',
		'subjects',
		'rootRepositories',
		'rivals'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'accessing' }
MetacelloMVSResolution >> add: aFinding about: aSubject [
	"Keep aFinding, found about aSubject (the version that asked, or whose baseline was read, or the user), and answer it"

	findings add: aFinding.
	subjects at: aFinding put: aSubject.
	^ aFinding
]

{ #category : 'accessing' }
MetacelloMVSResolution >> addMajorConflict: aFinding about: aCandidate against: anotherCandidate [
	"Keep aFinding, two majors of one repository: aCandidate's and anotherCandidate's, and answer it"

	rivals at: aFinding put: anotherCandidate.
	^ self add: aFinding about: aCandidate
]

{ #category : 'accessing' }
MetacelloMVSResolution >> addRequirement: aRequirement [

	requirements add: aRequirement.
	^ aRequirement
]

{ #category : 'accessing' }
MetacelloMVSResolution >> buildList [
	"Repository -> the MetacelloMVSCandidateVersion chosen for it"

	^ buildList
]

{ #category : 'accessing' }
MetacelloMVSResolution >> buildList: aDictionary [
	"The walk is over: each requirement remembers what its repository ended up with, and the warnings only about superseded versions stop being findings (their requirements keep them, as history)"

	buildList := aDictionary.
	requirements do: [ :each | each resolveIn: aDictionary ].
	findings := findings reject: [ :each | each isWarning and: [ self isOnlyAboutSuperseded: each ] ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> dependenciesOf: aRepository [
	"The repositories of the build list that the selected version of aRepository requires"

	^ ((self requirementsOfSelected: aRepository) collect: [ :each | each resolved ] thenSelect: [ :each | each notNil ])
		  collect: [ :each | each repository ]
		  as: Set
]

{ #category : 'accessing' }
MetacelloMVSResolution >> descriptionOf: aFinding [
	"Its text, and on the next line who required what it is about. A major conflict shows who asked each major, one per line"

	| subject |
	subject := self subjectOf: aFinding.
	rivals at: aFinding ifPresent: [ :rival |
			^ String lf join: {
					  aFinding text.
					  ('- ' , rival version string , ' required by ' , rival requiredBy chainLine).
					  ('- ' , subject version string , ' required by ' , subject requiredBy chainLine) } ].
	^ String streamContents: [ :stream |
			  stream nextPutAll: aFinding text.
			  subject printRequiredByOn: stream ]
]

{ #category : 'report' }
MetacelloMVSResolution >> descriptionOfFinding: aFinding [

	^ self descriptionOf: aFinding
]

{ #category : 'accessing' }
MetacelloMVSResolution >> errorsDescription [
	"Each error with its chain"

	^ String lf join: (self errors collect: [ :each | self descriptionOf: each ])
]

{ #category : 'accessing' }
MetacelloMVSResolution >> findingKind: aKind severity: aSeverity text: aString about: aSubject ifNone: aBlock [
	"The finding kept already with this kind, severity and text about the same repository, or the value of aBlock: each is reported once, with its first chain"

	^ findings
		  detect: [ :each |
			  each kind = aKind and: [
				  each severity = aSeverity and: [ each text = aString and: [ (self subjectOf: each) isAboutSameRepositoryAs: aSubject ] ] ] ]
		  ifNone: aBlock
]

{ #category : 'accessing' }
MetacelloMVSResolution >> findingsOf: aRepository [
	"The findings about the version selected (or visited) of aRepository"

	^ findings select: [ :each | (self subjectOf: each) isAboutRepository: aRepository ]
]

{ #category : 'initialization' }
MetacelloMVSResolution >> initialize [
	super initialize.
	buildList := Dictionary new.
	subjects := IdentityDictionary new.
	rivals := IdentityDictionary new.
	requirements := OrderedCollection new.
	rootRepositories := #(  )
]

{ #category : 'accessing' }
MetacelloMVSResolution >> isOnlyAboutSuperseded: aFinding [
	"Whether every requirement it was found in was asked by a superseded version or, found in no requirement (reading a baseline), it is about a version not selected"

	| holders |
	holders := requirements select: [ :each | each findings identityIncludes: aFinding ].
	holders ifEmpty: [ ^ (self subjectOf: aFinding) isSupersededIn: self ].
	^ holders allSatisfy: [ :each | self isSuperseded: each ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> isRoot: aCandidate [

	^ self rootCandidates includes: aCandidate
]

{ #category : 'accessing' }
MetacelloMVSResolution >> isSuperseded: aRequirement [
	"Whether it was asked by a version that lost to a higher one of its repository"

	^ aRequirement from isSupersededIn: self
]

{ #category : 'accessing' }
MetacelloMVSResolution >> isSupersededSelection: aCandidate [
	"Whether aCandidate is not the version its repository ended up with"

	^ (buildList at: aCandidate repository ifAbsent: [ ^ true ]) version ~= aCandidate version
]

{ #category : 'report' }
MetacelloMVSResolution >> kindDescriptions [

	^ {
		  (#assumedInstalled -> 'Counted as installed because its packages are present').
		  (#baselineFailure -> 'Baseline does not load').
		  (#chronologyInversion -> 'Versions dated out of order').
		  (#copiedProject -> 'Project required under a second name').
		  (#floatingVersion -> 'Floating version, taken as the first of its series').
		  (#heldByLock -> 'Kept at the locked version').
		  (#higherThanReplace -> 'Asks more than the replace gives').
		  (#ignoredReplace -> 'Replace ignored outside the root').
		  (#imageQuery -> 'Baseline asks the image').
		  (#invalidReference -> 'Not a repository with versions').
		  (#majorConflict -> 'Two majors of one repository').
		  (#missingBaseline -> 'Baseline not found').
		  (#missingGroup -> 'Group not declared').
		  (#missingVersion -> 'No version satisfies the minimum').
		  (#newerInMajor -> 'Newer version in the same major').
		  (#newerMajor -> 'Newer major').
		  (#nonCanonicalVersion -> 'Version not written vX.Y.Z').
		  (#platformBlock -> 'Baseline depends on the Pharo version').
		  (#platformMismatch -> 'Needs another Pharo version').
		  (#projectConflict -> 'One project from two repositories').
		  (#raisedByOthers -> 'Already selected at a newer version').
		  (#removedAPI -> 'Baseline uses an API removed from Pharo').
		  (#replaced -> 'Replaced').
		  (#repositoryFailure -> 'Repository cannot be read').
		  (#spelling -> 'Repository spelled with another capitalization').
		  (#taggedVersionAvailable -> 'No version asked, but the repository has versions').
		  (#unversionedReference -> 'No version asked').
		  (#writtenPath -> 'Repository string writes a path') } asDictionary
]

{ #category : 'report' }
MetacelloMVSResolution >> lineOfUpgrade: anUpgradeFinding [
	"The version it can ask for, instead of what it asks, and why when it is not just newer: Hashtable v1.0.5 instead of v1.x.x"

	| note |
	note := {
		        (#raisedByOthers -> ': already selected').
		        (#taggedVersionAvailable -> ': fixes an error').
		        (#newerMajor -> ': a newer major').
		        (#newerInMajor -> '') } asDictionary at: anUpgradeFinding kind.
	^ anUpgradeFinding project , ' ' , anUpgradeFinding version , ' instead of ' , anUpgradeFinding asked , note
]

{ #category : 'report' }
MetacelloMVSResolution >> print: someFindings of: aSeverity verbose: aBoolean on: aStream [
	"Verbose, infos are upgrades: grouped by who asks, in the order found, one line per dependency it can ask for (see lineOfUpgrade:)"

	| byAsker |
	(aSeverity = #info and: [ aBoolean ]) ifFalse: [
			^ super
				  print: someFindings
				  of: aSeverity
				  verbose: aBoolean
				  on: aStream ].
	byAsker := OrderedDictionary new.
	someFindings do: [ :each | (byAsker at: (self subjectOf: each) ifAbsentPut: [ OrderedCollection new ]) add: each ].
	byAsker keysAndValuesDo: [ :asker :upgrades |
			aStream
				lf;
				nextPutAll: asker displayName;
				nextPutAll: ' can ask for'.
			upgrades do: [ :each | self print: (self lineOfUpgrade: each) asFindingOn: aStream ] ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> printOn: aStream [

	aStream
		print: buildList size;
		nextPutAll: ' projects, ';
		print: self errors size;
		nextPutAll: ' errors, ';
		print: self warnings size;
		nextPutAll: ' warnings'
]

{ #category : 'accessing' }
MetacelloMVSResolution >> requirements [
	"Every requirement met, those of superseded versions included"

	^ requirements
]

{ #category : 'accessing' }
MetacelloMVSResolution >> requirementsOfSelected: aRepository [
	"The requirements the selected version of aRepository asked"

	^ requirements select: [ :each | (each from isAboutRepository: aRepository) and: [ (self isSuperseded: each) not ] ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> resolves [

	^ self hasErrors not
]

{ #category : 'accessing' }
MetacelloMVSResolution >> rootCandidates [
	"The candidates of the roots that could be selected, in order"

	^ rootRepositories select: [ :each | buildList includesKey: each ] thenCollect: [ :each | buildList at: each ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> rootRepositories: aCollection [
	"The repositories of the roots, in the order they were asked"

	rootRepositories := aCollection
]

{ #category : 'report' }
MetacelloMVSResolution >> shortDescriptionOf: aFinding [
	"One line naming where it is: the version that asked (Talents in Famix v1.2.1), or for a major conflict the version that asked each major"

	| subject |
	subject := self subjectOf: aFinding.
	rivals at: aFinding ifPresent: [ :rival |
			^ subject project , ' ' , rival version string , ' in ' , rival requiredBy displayName , ', ' , subject version string , ' in '
			  , subject requiredBy displayName ].
	^ subject locate: (super shortDescriptionOf: aFinding)
]

{ #category : 'accessing' }
MetacelloMVSResolution >> subjectOf: aFinding [
	"What aFinding was found about: the version that asked, or whose baseline was read, or the user for the load as a whole"

	^ subjects at: aFinding
]
