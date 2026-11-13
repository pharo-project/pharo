"
I test MetacelloMVSResolver as a Metacello resolver: Metacello scripts resolve, record and load the versions it selects. Repositories are written by hand in a MetacelloMVSRecordedSource and loading goes through MetacelloTestLoader, so nothing is fetched.
"
Class {
	#name : 'MetacelloMVSScriptTest',
	#superclass : 'MetacelloTestCase',
	#instVars : [
		'source'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'running' }
MetacelloMVSScriptTest >> baselineWithPackage: aPackageName requiring: aCollection [

	^ String lf join: {
			  'baseline: spec'.
			  '	<baseline>'.
			  '	spec for: #common do: ['.
			  (String lf join:
				   (aCollection collect: [ :each | '		spec baseline: ''' , each key , ''' with: [ spec repository: ''' , each value , ''' ].' ])).
			  ('		spec package: ''' , aPackageName , ''' with: [ spec requires: #('
			   , (' ' join: (aCollection collect: [ :each | each key printString ])) , ') ] ]') }
]

{ #category : 'running' }
MetacelloMVSScriptTest >> fooScript [

	^ Metacello new
		  baseline: 'Foo';
		  repository: 'github://acme/Foo:v1.0.0';
		  resolver: self resolver;
		  yourself
]

{ #category : 'running' }
MetacelloMVSScriptTest >> resolver [

	^ MetacelloMVSResolver on: (MetacelloMVSGraph new source: source)
]

{ #category : 'running' }
MetacelloMVSScriptTest >> setUp [

	super setUp.
	source := MetacelloMVSRecordedSource new.
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineWithPackage: 'Foo-Core' requiring: { ('Bar' -> 'github://acme/Bar:v1.1.0') }).
	#( 'v1.1.0' 'v1.2.0' 'v2.0.0' ) do: [ :each |
			source
				repository: 'github://acme/Bar'
				version: each
				project: 'Bar'
				baseline: (self baselineWithPackage: 'Bar-Core' requiring: #(  )) ]
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testALoadInstallsTheBaselinesReadInTheSandbox [

	| loaded |
	loaded := MetacelloTestLoader new.
	self fooScript
		loader: loaded;
		load.
	self
		assert: (loaded loaded collect: [ :each | each key ]) asArray
		equals: #( 'Bar-Core' 'Foo-Core' 'BaselineOfFoo' 'BaselineOfBar' )
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testALoadRegistersTheVersionsSelected [

	self fooScript
		loader: MetacelloTestLoader new;
		load.
	self
		assert: (MetacelloProjectRegistry
				 registrationNamed: 'BaselineOfBar'
				 ifPresent: [ :each | each repositoryDescriptions asArray ]
				 ifAbsent: [ nil ])
		equals: #( 'github://acme/Bar:v1.1.0' )
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testALoadWithErrorsLoadsNothing [

	| loaded error |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineWithPackage: 'Foo-Core' requiring: { ('Bar' -> 'github://acme/Bar:v9.0.0') }).
	loaded := MetacelloTestLoader new.
	error := [
		         self fooScript
			         loader: loaded;
			         load.
		         nil ]
		         on: MetacelloResolutionError
		         do: [ :e | e ].
	self assert: (error resolution errors anySatisfy: [ :each | each kind = #missingVersion ]).
	self assert: loaded loaded isEmpty
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testAScriptResolvesEveryBaselineItNamesTogether [

	| resolution |
	resolution := self fooScript
		              baseline: 'Bar' repository: 'github://acme/Bar:v1.2.0';
		              resolve.

	self assert: (resolution roots collect: [ :each | each name ]) asArray equals: #( 'Foo' 'Bar' ).
	self
		assert: (resolution projects collect: [ :each | each name -> each projectSpec repositoryDescriptions first ]) asDictionary
		equals: {
				('Foo' -> 'github://acme/Foo:v1.0.0').
				('Bar' -> 'github://acme/Bar:v1.2.0') } asDictionary
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testAVersionInstalledOnlyByTheLegacyRuleIsAWarning [
	"The registry has Metacello at v1.0.0, not marked as loaded; Metacello's packages are in the image, so the legacy rule counts it, and the resolution says so"

	| resolution |
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: (self baselineWithPackage: 'Foo-Core' requiring: { ('Metacello' -> 'github://acme/Metacello:v1.0.0') }).
	source
		repository: 'github://acme/Metacello'
		version: 'v1.0.0'
		project: 'Metacello'
		baseline: (self baselineWithPackage: 'Metacello-Core' requiring: #(  )).
	(MetacelloProjectRegistration projectSpec: ((MetacelloProjectSpec project: MetacelloProject new)
			  name: 'Metacello';
			  className: 'BaselineOfMetacello';
			  repository: 'github://acme/Metacello:v1.0.0';
			  yourself)) registerProject.

	resolution := self fooScript resolve.
	self assert: (resolution warnings anySatisfy: [ :each |
			 (each kind = #assumedInstalled) and: [ each text includesSubstring: 'github://acme/Metacello v1.0.0' ] ])
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testAVersionRegisteredAsLoadedIsNotAssumed [

	| resolution |
	((MetacelloProjectRegistration projectSpec: ((MetacelloProjectSpec project: MetacelloProject new)
				   name: 'Bar';
				   className: 'BaselineOfBar';
				   repository: 'github://acme/Bar:v1.2.0';
				   yourself))
		 loadedInImage: true;
		 yourself) registerProject.
	resolution := self fooScript resolve.
	self deny: (resolution warnings anySatisfy: [ :each | each kind = #assumedInstalled ])
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testAnInstalledVersionIsNeverDowngraded [
	"The registry says Bar v1.2.0 is loaded: Foo's minimum v1.1.0 does not bring it down"

	| resolution |
	((MetacelloProjectRegistration projectSpec: ((MetacelloProjectSpec project: MetacelloProject new)
				   name: 'Bar';
				   className: 'BaselineOfBar';
				   repository: 'github://acme/Bar:v1.2.0';
				   yourself))
		 loadedInImage: true;
		 yourself) registerProject.
	resolution := self fooScript resolve.
	self
		assert: (resolution projects detect: [ :each | each name = 'Bar' ]) projectSpec repositoryDescriptions first
		equals: 'github://acme/Bar:v1.2.0'
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testResolveAnswersTheProjectsAtTheVersionsSelected [

	| resolution |
	resolution := self fooScript resolve.
	self assert: resolution class equals: MetacelloMVSResolution.
	self assert: (resolution roots collect: [ :each | each name ]) asArray equals: #( 'Foo' ).
	self
		assert: (resolution projects collect: [ :each | each name -> each projectSpec repositoryDescriptions first ]) asDictionary
		equals: {
				('Foo' -> 'github://acme/Foo:v1.0.0').
				('Bar' -> 'github://acme/Bar:v1.1.0') } asDictionary
]

{ #category : 'tests' }
MetacelloMVSScriptTest >> testTheProjectsAreRecordedFromTheVersionsSelected [

	| loaded |
	loaded := MetacelloTestLoader new.
	self fooScript
		loader: loaded;
		load.
	self assert: (loaded loaded detect: [ :each | each key = 'Bar-Core' ]) value equals: #( 'github://acme/Bar:v1.1.0' ).
	self assert: (loaded loaded detect: [ :each | each key = 'Foo-Core' ]) value equals: #( 'github://acme/Foo:v1.0.0' )
]
