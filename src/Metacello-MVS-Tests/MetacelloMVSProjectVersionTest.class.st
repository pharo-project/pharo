"
I test what MVS reads from baselines (warnings, Pharo versions, replaces, groups, locks), on baselines written by hand in a MetacelloMVSRecordedSource.
"
Class {
	#name : 'MetacelloMVSProjectVersionTest',
	#superclass : 'MetacelloTestCase',
	#instVars : [
		'source',
		'graph'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'running' }
MetacelloMVSProjectVersionTest >> baselineRequiring: aCollection [
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
MetacelloMVSProjectVersionTest >> descriptionOf: aProjectSpec [

	^ aProjectSpec repositoryDescriptions first
]

{ #category : 'tests - groups' }
MetacelloMVSProjectVersionTest >> fooRequiring: aCollectionOfLines [

	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (String lf join:
				 #( 'baseline: spec' '	<baseline>' '	spec for: #common do: [' ) , aCollectionOfLines , #( '		spec package: ''Core'' ]' ))
]

{ #category : 'tests - groups' }
MetacelloMVSProjectVersionTest >> groupsSetUp [
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
MetacelloMVSProjectVersionTest >> installed: aDescription project: aName [
	"What the image has installed: the project spec it was loaded from, by the description aDescription"
	^ graph specFor: aDescription project: aName loads: #(  )
]

{ #category : 'tests - platform' }
MetacelloMVSProjectVersionTest >> pharoBaseline: aLine [

	^ String lf join: {
			  'baseline: spec'.
			  '	<baseline>'.
			  '	spec for: #common do: ['.
			  aLine.
			  '		spec package: ''Core'' ]' }
]

{ #category : 'running' }
MetacelloMVSProjectVersionTest >> projectOf: aProjectSpec [
	"The project a spec read from a baseline names: its baseline's"

	^ aProjectSpec className allButFirst: 'BaselineOf' size
]

{ #category : 'tests - dev' }
MetacelloMVSProjectVersionTest >> resolveInstalled: aBarMinimum [

	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> ('github://acme/Bar:' , aBarMinimum)) }).
	^ (MetacelloMVSResolver on: (MetacelloMVSGraph on: source))
		  installed: { ('Bar' -> (self installed: 'github://acme/Bar:main' project: 'Bar')) } asDictionary;
		  resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'
]

{ #category : 'tests - locks' }
MetacelloMVSProjectVersionTest >> resolveWithBarLockedAt: aLockedTag asking: aBarMinimum [

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
		  installed: { ('Bar' -> (self installed: 'github://acme/Bar:' , aLockedTag project: 'Bar')) } asDictionary;
		  locked: #( 'Bar' );
		  resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'
]

{ #category : 'running' }
MetacelloMVSProjectVersionTest >> selectionOf: aRepositoryString project: aProjectName [
	"The candidate for aRepositoryString at its minimum, for the project aProjectName"

	| parts spec resolver user |
	parts := MetacelloMVSGraph on: MetacelloMVSRecordedSource new.
	resolver := MetacelloMVSResolver on: parts.
	user := MetacelloUserRequest new.
	spec := parts specFor: aRepositoryString project: aProjectName loads: #(  ).
	^ resolver candidateFor: spec version: (resolver minimumOf: spec) requiredBy: user in: (MetacelloMVSRequirement from: user asked: spec)
]

{ #category : 'running' }
MetacelloMVSProjectVersionTest >> setUp [

	super setUp.
	source := MetacelloMVSRecordedSource new.
	graph := MetacelloMVSGraph on: source
]

{ #category : 'running' }
MetacelloMVSProjectVersionTest >> specRepositoryAt: someVersions [
	"github://acme/Spec holds two projects at each of someVersions: Spec2, whose baseline asks SpecCore from its own repository at the same version, and SpecCore, which asks nothing"

	someVersions do: [ :each |
			source
				repository: 'github://acme/Spec'
				version: each
				project: 'Spec2'
				baseline: (self baselineRequiring: { ('SpecCore' -> ('github://acme/Spec:' , each)) }).
			source
				repository: 'github://acme/Spec'
				version: each
				project: 'SpecCore'
				baseline: (self baselineRequiring: #(  )) ]
]

{ #category : 'tests' }
MetacelloMVSProjectVersionTest >> testABaselineThatAsksTheImageIsAWarning [

	| warnings |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (String lf join:
				 #( 'baseline: spec' '	<baseline>' '	spec for: #common do: [' '		(Smalltalk includesKey: #Foo) ifTrue: [ spec package: ''Extra'' ].'
				    '		spec package: ''Core'' ]' )).
	warnings := self warningsOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo').
	self assert: (warnings anySatisfy: [ :each | each = 'Smalltalk' ])
]

{ #category : 'tests - siblings' }
MetacelloMVSProjectVersionTest >> testADevProjectAndAVersionOfItsRepositoryAreAnError [
	"Spec2 in dev mode at master, SpecCore asked at v1.0.0: the repository would be at a ref and at a version"

	| report |
	self specRepositoryAt: #( 'v1.0.0' ).
	source
		repository: 'github://acme/Spec'
		version: 'master'
		project: 'Spec2'
		baseline: (self baselineRequiring: #(  )).
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('SpecCore' -> 'github://acme/Spec:v1.0.0') }).
	report := (MetacelloMVSResolver on: graph) resolveRoots: {
			          (graph specFor: 'github://acme/Foo:v1.0.0' project: 'Foo' loads: #(  )).
			          ((graph specFor: 'github://acme/Spec:master' project: 'Spec2' loads: #(  ))
				           beDev;
				           yourself) }.
	self assert: (report errors collect: [ :each | each kind ]) asArray equals: #( #siblingConflict )
]

{ #category : 'tests' }
MetacelloMVSProjectVersionTest >> testAGraphNeedsASource [
	self should: [ MetacelloMVSGraph new ] raise: Error
]

{ #category : 'tests' }
MetacelloMVSProjectVersionTest >> testAGraphReadsTheDatesOfTheSource [

	| date |
	date := DateAndTime year: 2020 month: 1 day: 1.
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: #(  )).
	source repository: 'github://acme/Foo' dates: { ('v1.0.0' -> date) } asDictionary.
	graph := MetacelloMVSGraph on: source.
	graph versionsOf: 'github://acme/Foo' ifFail: [ :text | self fail: text ].
	self assert: (graph datesOf: 'github://acme/Foo') equals: { ('v1.0.0' -> date) } asDictionary
]

{ #category : 'tests - groups' }
MetacelloMVSProjectVersionTest >> testAGroupAskedPullsWhatItNeeds [

	self groupsSetUp.
	self fooRequiring: #( '		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:v1.0.0''; loads: #(''Tests'') ].' ).

	self assert: (((MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo') buildList includesKey: 'Tester')
]

{ #category : 'tests - locks' }
MetacelloMVSProjectVersionTest >> testALockHoldsAgainstAHigherMinimumOfItsMajor [

	| report |
	report := self resolveWithBarLockedAt: 'v1.0.0' asking: 'v1.2.0'.
	self
		assert: report errors isEmpty;
		assert: (report buildList at: 'Bar') version string equals: 'v1.0.0';
		assert: (report warnings anySatisfy: [ :each | each kind = #heldByLock ])
]

{ #category : 'tests - locks' }
MetacelloMVSProjectVersionTest >> testALockOfAnotherMajorIsAnError [

	| report |
	report := self resolveWithBarLockedAt: 'v1.0.0' asking: 'v2.0.0'.
	self
		assert: (report buildList at: 'Bar') version string equals: 'v1.0.0';
		assert: (report errors anySatisfy: [ :each | (each kind = #majorConflict) and: [ each text includesSubstring: 'locked' ] ])
]

{ #category : 'tests - platform' }
MetacelloMVSProjectVersionTest >> testAPharoBlockIsAWarning [

	| warnings |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self pharoBaseline: '		spec for: #''pharo14.x'' do: [ spec package: ''Old'' ].').
	warnings := self warningsOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo').
	self assert: (warnings anySatisfy: [ :each | each includesSubstring: 'pharo14.x' ])
]

{ #category : 'tests - siblings' }
MetacelloMVSProjectVersionTest >> testAProjectIsRaisedToAHigherProjectOfItsRepository [
	"Foo asks SpecCore v1.1.0 first, then Spec2 v1.0.0: Spec2 takes v1.1.0 directly"

	| report |
	self specRepositoryAt: #( 'v1.0.0' 'v1.1.0' ).
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: {
					 ('SpecCore' -> 'github://acme/Spec:v1.1.0').
					 ('Spec2' -> 'github://acme/Spec:v1.0.0') }).
	report := (MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self assert: report resolves.
	self assert: (report buildList at: 'Spec2') version string equals: 'v1.1.0'.
	self assert: (report buildList at: 'SpecCore') version string equals: 'v1.1.0'
]

{ #category : 'tests - siblings' }
MetacelloMVSProjectVersionTest >> testAProjectIsRaisedWithAnotherProjectOfItsRepository [
	"Foo asks Spec2 v1.0.0 (which asks SpecCore v1.0.0) and SpecCore v1.1.0: Spec2 is raised to v1.1.0, one version of the repository"

	| report |
	self specRepositoryAt: #( 'v1.0.0' 'v1.1.0' ).
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: {
					 ('Spec2' -> 'github://acme/Spec:v1.0.0').
					 ('SpecCore' -> 'github://acme/Spec:v1.1.0') }).
	report := (MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self assert: report resolves.
	self assert: (report buildList associations collect: [ :each | each key -> each value version string ]) asDictionary equals: {
			('Foo' -> 'v1.0.0').
			('Spec2' -> 'v1.1.0').
			('SpecCore' -> 'v1.1.0') } asDictionary.
	self
		assert: ((report infos select: [ :each | each kind = #raisedWithSibling ]) collect: [ :each | each text ]) asArray
		equals: #( 'Spec2 v1.0.0 is raised to v1.1.0 with SpecCore, from one repository' )
]

{ #category : 'tests' }
MetacelloMVSProjectVersionTest >> testAnAliasIsNamedAfterItsBaseline [
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
	self assert: ((graph projectVersionOf: selection) references collect: [ :each | self projectOf: each ]) asSet equals: #( 'Bar' ) asSet.
	self assert: ((self warningsOf: selection) anySatisfy: [ :each | each = 'project:copyFrom:with:' ])
]

{ #category : 'tests - locks' }
MetacelloMVSProjectVersionTest >> testAnInstalledVersionThatIsNotLockedIsRaised [

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
	self assert: (report buildList at: 'Bar') version string equals: 'v1.2.0'
]

{ #category : 'tests - groups' }
MetacelloMVSProjectVersionTest >> testGroupsAskedAddUpAndTheVersionIsReadAgain [
	"Foo asks Bar's default groups, then its alias BarTests asks Bar's Tests: Bar is read again for both"

	| report |
	self groupsSetUp.
	self fooRequiring: #( '		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:v1.0.0'' ].'
		   '		spec project: ''BarTests'' copyFrom: ''Bar'' with: [ spec loads: #(''Tests'') ].'
		   '		spec package: ''Foo-Tests'' with: [ spec requires: #(''BarTests'') ].'
		   '		spec group: ''default'' with: #(''Core'' ''Foo-Tests'').' ).

	report := (MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self assert: (report buildList includesKey: 'Tester').
	self assert: (report requirements select: [ :each | each repository = 'github://acme/Tester' ]) size equals: 1
]

{ #category : 'tests - replace' }
MetacelloMVSProjectVersionTest >> testReadsTheReplacesOfABaseline [

	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self pharoBaseline: '		spec replace: ''Bloc'' with: ''github://me/Bloc:v3.0.1-fix/src''.').
	self
		assert: (graph projectVersionOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo')) replacements
		equals: { ('Bloc' -> 'github://me/Bloc:v3.0.1-fix/src') } asDictionary
]

{ #category : 'tests' }
MetacelloMVSProjectVersionTest >> testReadsTheRequirementsFromATonelBaseline [

	| requirements |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> 'github://acme/Bar:v1.2.0') }).
	requirements := (graph projectVersionOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo')) references.
	self assert: (requirements collect: [ :each | self descriptionOf: each ]) asArray equals: #( 'github://acme/Bar:v1.2.0' )
]

{ #category : 'tests' }
MetacelloMVSProjectVersionTest >> testResolvesThroughAGraph [

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
	buildList := ((MetacelloMVSResolver on: (MetacelloMVSGraph on: source)) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo') buildList.
	self assert: (buildList at: 'Bar') version string equals: 'v1.1.0'
]

{ #category : 'tests - groups' }
MetacelloMVSProjectVersionTest >> testTheDefaultGroupsLeaveOutWhatOnlyTestsNeed [

	self groupsSetUp.
	self fooRequiring: #( '		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:v1.0.0'' ].' ).

	self deny: (((MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo') buildList includesKey: 'Tester')
]

{ #category : 'tests - groups' }
MetacelloMVSProjectVersionTest >> testTheRootLoadsTheGroupsAsked [

	self groupsSetUp.
	self
		deny: (((MetacelloMVSResolver on: graph) resolve: 'github://acme/Bar:v1.0.0' project: 'Bar' loads: #(  )) buildList includesKey: 'Tester');
		assert:
			(((MetacelloMVSResolver on: graph) resolve: 'github://acme/Bar:v1.0.0' project: 'Bar' loads: #( 'Tests' )) buildList includesKey: 'Tester')
]

{ #category : 'tests - siblings' }
MetacelloMVSProjectVersionTest >> testTwoMajorsOfOneRepositoryAreAnErrorAcrossProjects [

	| report |
	self specRepositoryAt: #( 'v1.0.0' 'v2.0.0' ).
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: {
					 ('SpecCore' -> 'github://acme/Spec:v2.0.0').
					 ('Spec2' -> 'github://acme/Spec:v1.0.0') }).
	report := (MetacelloMVSResolver on: graph) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self deny: report resolves.
	self assert: (report errors anySatisfy: [ :each | each kind = #siblingConflict or: [ each kind = #majorConflict ] ])
]

{ #category : 'tests' }
MetacelloMVSProjectVersionTest >> testTwoProjectsOfOneRepositoryAreReadEachFromItsBaseline [
	"Like Spec2 asking SpecCore from its own repository: each project is read from its own baseline, not from the other's"

	| report |
	source
		repository: 'github://acme/Spec'
		version: 'v1.0.0'
		project: 'Spec2'
		baseline: (self baselineRequiring: { ('SpecCore' -> 'github://acme/Spec:v1.0.0') }).
	source
		repository: 'github://acme/Spec'
		version: 'v1.0.0'
		project: 'SpecCore'
		baseline: (self baselineRequiring: { ('Tester' -> 'github://acme/Tester:v1.0.0') }).
	source
		repository: 'github://acme/Tester'
		version: 'v1.0.0'
		project: 'Tester'
		baseline: (self baselineRequiring: #(  )).
	report := (MetacelloMVSResolver on: graph) resolve: 'github://acme/Spec:v1.0.0' project: 'Spec2'.
	self assert: report resolves.
	self assert: report buildList keys asSortedCollection asArray equals: #( 'Spec2' 'SpecCore' 'Tester' )
]

{ #category : 'running' }
MetacelloMVSProjectVersionTest >> warningsOf: aCandidate [
	"The texts of the warnings reading the baseline of aCandidate gives"

	^ ((graph projectVersionOf: aCandidate) findings select: [ :each | each isWarning ]) collect: [ :each | each text ]
]
