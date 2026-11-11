"
I am the resolution mvs answers: what the walk found, as far as it could go, and, when it resolves, the projects for Metacello to record (see MetacelloResolution).

buildList: repository -> the MetacelloMVSCandidate chosen.
requirements: every MetacelloMVSRequirement met: what was asked, by which version, and what it resolved to.
rootRepository: the repository of the root.
subjects: finding -> the candidate it was found about, for its repository and its chain.

I resolve when I have no errors; with errors, the build list is incomplete and I hold no projects.
"
Class {
	#name : 'MetacelloMVSResolution',
	#superclass : 'MetacelloResolution',
	#instVars : [
		'buildList',
		'requirements',
		'rootRepository',
		'subjects'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'accessing' }
MetacelloMVSResolution >> add: aFinding about: aCandidateOrNil [
	"Keep aFinding, found about aCandidateOrNil (the version that asked, or whose baseline was read), and answer it"

	findings add: aFinding.
	subjects at: aFinding put: aCandidateOrNil.
	^ aFinding
]

{ #category : 'accessing' }
MetacelloMVSResolution >> addRequirement: aRequirement [

	requirements add: aRequirement.
	^ aRequirement
]

{ #category : 'accessing' }
MetacelloMVSResolution >> buildList [
	"Repository -> the MetacelloMVSCandidate chosen for it"

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
	"Its text, and the chain of versions that led to it"

	^ (self subjectOf: aFinding)
		  ifNil: [ aFinding text ]
		  ifNotNil: [ :candidate | aFinding text , '. Required by:' , String lf , candidate chainString ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> errorsDescription [
	"Each error with its chain"

	^ String lf join: (self errors collect: [ :each | self descriptionOf: each ])
]

{ #category : 'accessing' }
MetacelloMVSResolution >> findingKind: aKind severity: aSeverity text: aString about: aCandidateOrNil [
	"The finding kept already with this kind, severity and text about the same repository, or nil: each is reported once, with its first chain"

	| repository |
	repository := aCandidateOrNil ifNotNil: [ aCandidateOrNil repository ].
	^ findings
		  detect: [ :each |
			  each kind = aKind and: [
				  each severity = aSeverity and: [ each text = aString and: [ (self repositoryOf: each) = repository ] ] ] ]
		  ifNone: [ nil ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> findingsOf: aRepository [
	"The findings about the version selected (or visited) of aRepository"

	^ findings select: [ :each | (self repositoryOf: each) = aRepository ]
]

{ #category : 'initialization' }
MetacelloMVSResolution >> initialize [

	super initialize.
	buildList := Dictionary new.
	subjects := IdentityDictionary new.
	requirements := OrderedCollection new
]

{ #category : 'accessing' }
MetacelloMVSResolution >> isOnlyAboutSuperseded: aFinding [
	"Whether every requirement it was found in was asked by a superseded version or, found in no requirement (reading a baseline), it is about a version not selected"

	| holders |
	holders := requirements select: [ :each | each findings identityIncludes: aFinding ].
	holders ifEmpty: [ ^ (self subjectOf: aFinding) notNil and: [ self isSupersededSelection: (self subjectOf: aFinding) ] ].
	^ holders allSatisfy: [ :each | self isSuperseded: each ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> isSuperseded: aRequirement [
	"Whether it was asked by a version that lost to a higher one of its repository"

	^ self isSupersededSelection: aRequirement from
]

{ #category : 'accessing' }
MetacelloMVSResolution >> isSupersededSelection: aCandidate [
	"Whether aCandidate is not the version its repository ended up with"

	^ (buildList at: aCandidate repository ifAbsent: [ ^ true ]) version ~= aCandidate version
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
MetacelloMVSResolution >> repositoryOf: aFinding [

	^ (self subjectOf: aFinding) ifNotNil: [ :candidate | candidate repository ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> requirements [
	"Every requirement met, those of superseded versions included"

	^ requirements
]

{ #category : 'accessing' }
MetacelloMVSResolution >> requirementsOfSelected: aRepository [
	"The requirements the selected version of aRepository asked"

	^ requirements select: [ :each | each from repository = aRepository and: [ (self isSuperseded: each) not ] ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> resolves [

	^ self hasErrors not
]

{ #category : 'accessing' }
MetacelloMVSResolution >> rootCandidate [
	"The candidate of the root, or nil when it could not be selected"

	^ buildList at: rootRepository ifAbsent: [ nil ]
]

{ #category : 'accessing' }
MetacelloMVSResolution >> rootRepository: aRepository [

	rootRepository := aRepository
]

{ #category : 'accessing' }
MetacelloMVSResolution >> subjectOf: aFinding [
	"The version aFinding was found about: the one that asked, or whose baseline was read; nil for the root's own spec"

	^ subjects at: aFinding ifAbsent: [ nil ]
]
