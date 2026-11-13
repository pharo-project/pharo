"
I test what MVS reads from baselines (warnings, Pharo versions, replaces, groups, locks), on baselines written by hand in a MetacelloMVSRecordedSource.
"
Class {
	#name : 'MetacelloMVSBaselineReadingTest',
	#superclass : 'MetacelloTestCase',
	#instVars : [
		'source',
		'graph'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'running' }
MetacelloMVSBaselineReadingTest >> baselineRequiring: aCollection [
	"The source of a baseline: method requiring each project named -> repository string in aCollection"

	^ String lf join: {
			  'baseline: spec'.
			  '	<baseline>'.
			  '	spec for: #common do: ['.
			  (String lf join:
				   (aCollection collect: [ :each | '		spec baseline: ''' , each key , ''' with: [ spec repository: ''' , each value , ''' ].' ])).
			  '		spec package: ''Core'' ]' }
]

{ #category : 'running' }
MetacelloMVSBaselineReadingTest >> descriptionOf: aProjectSpec [

	^ aProjectSpec repositoryDescriptions first
]

{ #category : 'tests - groups' }
MetacelloMVSBaselineReadingTest >> fooRequiring: aCollectionOfLines [

	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (String lf join:
				 #( 'baseline: spec' '	<baseline>' '	spec for: #common do: [' ) , aCollectionOfLines , #( '		spec package: ''Core'' ]' ))
]

{ #category : 'tests - groups' }
MetacelloMVSBaselineReadingTest >> groupsSetUp [
	"Bar's default group needs nothing; its Tests group needs Tester"

	source
		repository: 'github://acme/Bar'
		version: 'v1.0.0'
		project: 'Bar'
		baseline: (String lf join:
				 #( 'baseline: spec' '	<baseline>' '	spec for: #common do: [' '		spec baseline: ''Tester'' with: [ spec repository: ''github://acme/Tester:v1.0.0'' ].'
				    '		spec package: ''Bar-Core''.' '		spec package: ''Bar-Tests'' with: [ spec requires: #(''Bar-Core'' ''Tester'') ].'
				    '		spec group: ''default'' with: #(''Bar-Core'').' '		spec group: ''Tests'' with: #(''Bar-Tests'') ]' )).
	source
		repository: 'github://acme/Tester'
		version: 'v1.0.0'
		project: 'Tester'
		baseline: (self baselineRequiring: #(  ))
]

{ #category : 'helpers' }
MetacelloMVSBaselineReadingTest >> installed: aDescription project: aName [
	"What the image has installed: the project spec it was loaded from, by the description aDescription"
	^ graph specFor: aDescription project: aName loads: #(  )
]

{ #category : 'tests - platform' }
MetacelloMVSBaselineReadingTest >> pharoBaseline: aLine [

	^ String lf join: {
			  'baseline: spec'.
			  '	<baseline>'.
			  '	spec for: #common do: ['.
			  aLine.
			  '		spec package: ''Core'' ]' }
]

{ #category : 'running' }
MetacelloMVSBaselineReadingTest >> projectOf: aProjectSpec [
	"The project a spec read from a baseline names: its baseline's"

	^ aProjectSpec className allButFirst: 'BaselineOf' size
]

{ #category : 'tests - dev' }
MetacelloMVSBaselineReadingTest >> resolveInstalled: aBarMinimum [

	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> ('github://acme/Bar:' , aBarMinimum)) }).
	^ (MetacelloMVSResolver on: (MetacelloMVSGraph new source: source))
		  installed: { ('github://acme/Bar' -> (self installed: 'github://acme/Bar:main' project: 'Bar')) } asDictionary;
		  resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'
]

{ #category : 'tests - locks' }
MetacelloMVSBaselineReadingTest >> resolveWithBarLockedAt: aLockedTag asking: aBarMinimum [

	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> ('github://acme/Bar:' , aBarMinimum)) }).
	#( 'v1.0.0' 'v1.2.0' 'v2.0.0' ) do: [ :each |
			source
				repository: 'github://acme/Bar'
				version: each
				project: 'Bar'
				baseline: (self baselineRequiring: #(  )) ].
	^ (MetacelloMVSResolver on: graph)
		  installed: { ('github://acme/Bar' -> (self installed: 'github://acme/Bar:' , aLockedTag project: 'Bar')) } asDictionary;
		  locked: #( 'github://acme/Bar' );
		  resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'
]

{ #category : 'running' }
MetacelloMVSBaselineReadingTest >> selectionOf: aRepositoryString project: aProjectName [
	"The candidate for aRepositoryString at its minimum, for the project aProjectName"

	| parts spec resolver |
	parts := MetacelloMVSGraph new.
	resolver := MetacelloMVSResolver on: parts.
	spec := parts specFor: aRepositoryString project: aProjectName loads: #(  ).
	^ resolver candidateFor: spec version: (resolver minimumOf: spec) requiredBy: nil
]

{ #category : 'running' }
MetacelloMVSBaselineReadingTest >> setUp [

	super setUp.
	source := MetacelloMVSRecordedSource new.
	graph := MetacelloMVSGraph new source: source
]

{ #category : 'tests' }
MetacelloMVSBaselineReadingTest >> testABaselineThatAsksTheImageIsAWarning [

	| warnings |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (String lf join:
				 #( 'baseline: spec' '	<baseline>' '	spec for: #common do: [' '		(Smalltalk includesKey: #Foo) ifTrue: [ spec package: ''Extra'' ].'
				    '		spec package: ''Core'' ]' )).
	warnings := self warningsOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo').
	self assert: (warnings anySatisfy: [ :each | each includesSubstring: 'asks the image' ])
]

{ #category : 'tests' }
MetacelloMVSBaselineReadingTest >> testAGraphReadsTheDatesOfTheSource [
	| date graph |
	date := DateAndTime year: 2020 month: 1 day: 1.
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: #(  )).
	source repository: 'github://acme/Foo' dates: { ('v1.0.0' -> date) } asDictionary.
	graph := MetacelloMVSGraph new source: source.
	graph versionsOf: 'github://acme/Foo' ifFail: [ :text | self fail: text ].
	self assert: (graph datesOf: 'github://acme/Foo') equals: { ('v1.0.0' -> date) } asDictionary
]

{ #category : 'tests - groups' }
MetacelloMVSBaselineReadingTest >> testAGroupAskedPullsWhatItNeeds [

	self groupsSetUp.
	self fooRequiring: #( '		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:v1.0.0''; loads: #(''Tests'') ].' ).

	self assert: (((MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo') buildList includesKey: 'github://acme/Tester')
]

{ #category : 'tests - locks' }
MetacelloMVSBaselineReadingTest >> testALockHoldsAgainstAHigherMinimumOfItsMajor [

	| report |
	report := self resolveWithBarLockedAt: 'v1.0.0' asking: 'v1.2.0'.
	self
		assert: report errors isEmpty;
		assert: (report buildList at: 'github://acme/Bar') version string equals: 'v1.0.0';
		assert: (report warnings anySatisfy: [ :each | each kind = #heldByLock ])
]

{ #category : 'tests - locks' }
MetacelloMVSBaselineReadingTest >> testALockOfAnotherMajorIsAnError [

	| report |
	report := self resolveWithBarLockedAt: 'v1.0.0' asking: 'v2.0.0'.
	self
		assert: (report buildList at: 'github://acme/Bar') version string equals: 'v1.0.0';
		assert: (report errors anySatisfy: [ :each | (each kind = #majorConflict) and: [ each text includesSubstring: 'locked' ] ])
]

{ #category : 'tests - platform' }
MetacelloMVSBaselineReadingTest >> testANewerPharoMinimumIsAnError [

	graph pharoMajor: 13.
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self pharoBaseline: '		spec pharo: 14.').
	self assert: ((graph readingOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo')) findings anySatisfy: [ :each |
			 each isError and: [ each kind = #platformMismatch ] ])
]

{ #category : 'tests - platform' }
MetacelloMVSBaselineReadingTest >> testAPharoBlockIsAWarning [

	| warnings |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self pharoBaseline: '		spec for: #''pharo14.x'' do: [ spec package: ''Old'' ].').
	warnings := self warningsOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo').
	self assert: (warnings anySatisfy: [ :each | each includesSubstring: 'pharo14.x' ])
]

{ #category : 'tests' }
MetacelloMVSBaselineReadingTest >> testAnAliasIsNamedAfterItsBaseline [
	"XMLWriter names OrderPreservingDictionary through project:copyFrom:with:, which is accepted with a warning"

	| selection |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (String lf join:
				 #( 'baseline: spec' '	<baseline>' '	spec for: #common do: [' '		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:v1.2.0'' ].'
				    '		spec project: ''BarCore'' copyFrom: ''Bar'' with: [ spec loads: #(''Core'') ].'
				    '		spec package: ''Core'' with: [ spec requires: #(''BarCore'') ] ]' )).
	selection := self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self assert: ((graph readingOf: selection) references collect: [ :each | self projectOf: each ]) asSet equals: #( 'Bar' ) asSet.
	self assert: ((self warningsOf: selection) anySatisfy: [ :each | each includesSubstring: 'project:copyFrom:with:' ])
]

{ #category : 'tests - locks' }
MetacelloMVSBaselineReadingTest >> testAnInstalledVersionThatIsNotLockedIsRaised [

	| report |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> 'github://acme/Bar:v1.2.0') }).
	#( 'v1.0.0' 'v1.2.0' ) do: [ :each |
			source
				repository: 'github://acme/Bar'
				version: each
				project: 'Bar'
				baseline: (self baselineRequiring: #(  )) ].
	report := (MetacelloMVSResolver on: graph)
		          installed: { ('github://acme/Bar' -> (self installed: 'github://acme/Bar:v1.0.0' project: 'Bar')) } asDictionary;
		          resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self assert: (report buildList at: 'github://acme/Bar') version string equals: 'v1.2.0'
]

{ #category : 'tests - platform' }
MetacelloMVSBaselineReadingTest >> testAnOlderPharoMinimumResolves [

	graph pharoMajor: 15.
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self pharoBaseline: '		spec pharo: 14.').
	self assert: (graph readingOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo')) references isEmpty
]

{ #category : 'tests - groups' }
MetacelloMVSBaselineReadingTest >> testGroupsAskedAddUpAndTheVersionIsReadAgain [
	"Foo asks Bar's default groups, then its alias BarTests asks Bar's Tests: Bar is read again for both"

	| report |
	self groupsSetUp.
	self fooRequiring: #( '		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:v1.0.0'' ].'
		   '		spec project: ''BarTests'' copyFrom: ''Bar'' with: [ spec loads: #(''Tests'') ].'
		   '		spec package: ''Foo-Tests'' with: [ spec requires: #(''BarTests'') ].'
		   '		spec group: ''default'' with: #(''Core'' ''Foo-Tests'').' ).

	report := (MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self assert: (report buildList includesKey: 'github://acme/Tester').
	self assert: (report requirements select: [ :each | each repository = 'github://acme/Tester' ]) size equals: 1
]

{ #category : 'tests - replace' }
MetacelloMVSBaselineReadingTest >> testReadsTheReplacesOfABaseline [

	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self pharoBaseline: '		spec replace: ''Bloc'' with: ''github://me/Bloc:v3.0.1-fix/src''.').
	self
		assert: (graph readingOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo')) replacements
		equals: { ('Bloc' -> 'github://me/Bloc:v3.0.1-fix/src') } asDictionary
]

{ #category : 'tests' }
MetacelloMVSBaselineReadingTest >> testReadsTheRequirementsFromATonelBaseline [

	| requirements |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> 'github://acme/Bar:v1.2.0') }).
	requirements := (graph readingOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo')) references.
	self assert: (requirements collect: [ :each | self descriptionOf: each ]) asArray equals: #( 'github://acme/Bar:v1.2.0' )
]

{ #category : 'tests' }
MetacelloMVSBaselineReadingTest >> testResolvesThroughAGraph [

	| buildList |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> 'github://acme/Bar:v1.1.0') }).
	source
		repository: 'github://acme/Bar'
		version: 'v1.1.0'
		project: 'Bar'
		baseline: (self baselineRequiring: #(  )).
	source
		repository: 'github://acme/Bar'
		version: 'v1.2.0'
		project: 'Bar'
		baseline: (self baselineRequiring: #(  )).
	buildList := ((MetacelloMVSResolver on: (MetacelloMVSGraph new source: source)) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo') buildList.
	self assert: (buildList at: 'github://acme/Bar') version string equals: 'v1.1.0'
]

{ #category : 'tests - groups' }
MetacelloMVSBaselineReadingTest >> testTheDefaultGroupsLeaveOutWhatOnlyTestsNeed [

	self groupsSetUp.
	self fooRequiring: #( '		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:v1.0.0'' ].' ).

	self deny: (((MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo') buildList includesKey: 'github://acme/Tester')
]

{ #category : 'tests - groups' }
MetacelloMVSBaselineReadingTest >> testTheRootLoadsTheGroupsAsked [

	self groupsSetUp.
	self
		deny: (((MetacelloMVSResolver on: graph) resolve: 'github://acme/Bar:v1.0.0' project: 'Bar' loads: #(  )) buildList includesKey:
					 'github://acme/Tester');
		assert:
			(((MetacelloMVSResolver on: graph) resolve: 'github://acme/Bar:v1.0.0' project: 'Bar' loads: #( 'Tests' )) buildList includesKey:
					 'github://acme/Tester')
]

{ #category : 'running' }
MetacelloMVSBaselineReadingTest >> warningsOf: aCandidate [
	"The texts of the warnings reading the baseline of aCandidate gives"

	^ ((graph readingOf: aCandidate) findings select: [ :each | each isWarning ]) collect: [ :each | each text ]
]
