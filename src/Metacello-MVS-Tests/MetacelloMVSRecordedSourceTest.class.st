Class {
	#name : 'MetacelloMVSRecordedSourceTest',
	#superclass : 'MetacelloMVSBaselineReadingTest',
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'tests' }
MetacelloMVSRecordedSourceTest class >> shouldInheritSelectors [

	^ false
]

{ #category : 'tests' }
MetacelloMVSRecordedSourceTest >> replayOf: aRootString [
	"Resolve aRootString (github://acme/Name:version, whose project is Name) recording what the source written by hand answers, and answer the recorded source rebuilt from its code"

	| recording |
	recording := MetacelloMVSRecordingSource new source: source.
	(MetacelloMVSResolver on: (MetacelloMVSGraph new source: recording))
		resolve: aRootString
		project: ((aRootString copyAfterLast: $/) copyUpTo: $:).
	^ self class compiler evaluate: recording recorded storeString
]

{ #category : 'tests' }
MetacelloMVSRecordedSourceTest >> testReplaysAFailingBaseline [

	| replayed reading |
	source repository: 'github://acme/Foo' versions: #( 'v1.0.0' ).
	source
		repository: 'github://acme/Foo'
		at: 'v1.0.0'
		baseline: (MetacelloMVSBaselineDefinitions error: #missingBaseline text: 'github://acme/Foo v1.0.0 has no BaselineOfFoo').
	replayed := self replayOf: 'github://acme/Foo:v1.0.0'.
	reading := (MetacelloMVSGraph new source: replayed) readingOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo').
	self assert: (reading findings anySatisfy: [ :each | each isError and: [ each kind = #missingBaseline ] ])
]

{ #category : 'tests' }
MetacelloMVSRecordedSourceTest >> testReplaysTheResolution [

	| replayed report |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineRequiring: { ('Bar' -> 'github://acme/Bar:v1.1.x') }).
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
	source repository: 'github://acme/Bar' dates: { ('v1.1.0' -> (DateAndTime year: 2020 month: 1 day: 1)) } asDictionary.
	replayed := self replayOf: 'github://acme/Foo:v1.0.0'.
	report := (MetacelloMVSResolver on: (MetacelloMVSGraph new source: replayed)) resolve: 'github://acme/Foo:v1.0.0' project: 'Foo'.
	self assert: report errors isEmpty.
	self assert: (report warnings anySatisfy: [ :each | each kind = #floatingVersion ]).
	self assert: report buildList size equals: 2.
	self assert: ((replayed datesOf: 'github://acme/Bar') at: 'v1.1.0') equals: (DateAndTime year: 2020 month: 1 day: 1)
]

{ #category : 'tests' }
MetacelloMVSRecordedSourceTest >> testStoresItsBaselines [

	| recorded copy baseline reading |
	recorded := MetacelloMVSRecordedSource new
		            repository: 'github://acme/Foo' versions: #( 'v1.0.0' );
		            repository: 'github://acme/Bar' failsWith: 'no network';
		            repository: 'github://acme/Foo'
		            at: 'v1.0.0'
		            class: #( 'BaselineOfFoo' 'BaselineOf' #(  ) #( 'Version' ) #(  ) )
		            methods: { {
				            false.
				            'baselines'.
				            (self baselineRequiring: { ('Bar' -> 'github://acme/Bar:v1.0.0') }) } }
		            readFrom: 'github://acme/Foo:v1.0.0/src'
		            problems: #( #( #writtenPath #warning 'writes the path src' ) );
		            repository: 'github://acme/Baz' at: 'v1.0.0' unreadable: #( #( #missingBaseline #error 'no baseline' ) );
		            yourself.
	copy := Smalltalk compiler evaluate: recorded storeString.
	baseline := copy baselineOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo').
	self
		assert: baseline repositoryDescription equals: 'github://acme/Foo:v1.0.0/src';
		assert: (baseline definitions detect: [ :each | each isClassDefinition ]) classVarNames asArray equals: #( #Version );
		assert: baseline problems first equals: #( #writtenPath #warning 'writes the path src' );
		assert: (copy versionsOf: 'github://acme/Bar' ifFail: [ :text | text ]) equals: 'no network';
		deny: (copy baselineOf: (self selectionOf: 'github://acme/Baz:v1.0.0' project: 'Baz')) hasBaseline.
	reading := (MetacelloMVSGraph new source: copy) readingOf: (self selectionOf: 'github://acme/Foo:v1.0.0' project: 'Foo').
	self assert: (reading references collect: [ :each | self descriptionOf: each ]) asArray equals: #( 'github://acme/Bar:v1.0.0' )
]
