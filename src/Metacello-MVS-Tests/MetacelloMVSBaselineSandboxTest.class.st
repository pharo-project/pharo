Class {
	#name : 'MetacelloMVSBaselineSandboxTest',
	#superclass : 'MetacelloTestCase',
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> baselineMethod: bodyLines [

	^ String lf join:
		  { ''. '{ #category : ''baselines'' }'. 'BaselineOfFoo >> baseline: spec ['. '<baseline>' } , bodyLines , { ']' }
]

{ #category : 'running' }
MetacelloMVSBaselineSandboxTest >> descriptionOf: aProjectSpec [

	^ aProjectSpec repositoryDescriptions first
]

{ #category : 'running' }
MetacelloMVSBaselineSandboxTest >> projectOf: aProjectSpec [
	"The project a spec read from a baseline names: its baseline's"

	^ aProjectSpec className allButFirst: 'BaselineOf' size
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> requirementsOf: bodyLines [

	^ (self sandboxOnTonel: (self tonelOf: { (self baselineMethod: bodyLines) })) requirementsInto: MetacelloMVSBaselineReading new
]

{ #category : 'running' }
MetacelloMVSBaselineSandboxTest >> sandboxOnTonel: aString [
	"A sandbox on the BaselineOf class of the Tonel file aString"

	^ MetacelloMVSBaselineSandbox onDefinitions: (TonelParser parseString: aString)
]

{ #category : 'running' }
MetacelloMVSBaselineSandboxTest >> sandboxOnTonel: aString repository: aRepositoryString [

	^ MetacelloMVSBaselineSandbox onDefinitions: (TonelParser parseString: aString) repository: aRepositoryString
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testABaselineOfTheSameRepositoryStaysAtTheReadVersion [
	"Like Spec2 declaring SpecCore with the repository it is loaded from"

	| tonel requirements |
	tonel := self tonelOf: { (self baselineMethod:
			          #( 'spec for: #common do: [ spec baseline: ''Core'' with: [ spec repository: ((self packageRepositoryURLForSpec: spec) ifEmpty: [ ''github://o/foo:main/src'' ]) ] ]' )) }.
	requirements := (self sandboxOnTonel: tonel repository: 'github://o/foo:v2.0.6/src') requirementsInto: MetacelloMVSBaselineReading new.
	self assert: (self descriptionOf: requirements first) equals: 'github://o/foo:v2.0.6/src'
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testABaselineOverridingPackageRepositoryURLCanCallSuper [
	"Like Famix: packageRepositoryURL ^ super packageRepositoryURL ifEmpty: [ ... ]"

	| tonel requirements |
	tonel := self tonelOf: {
			         (self baselineMethod:
				          #( 'spec for: #common do: [ spec baseline: ''Core'' with: [ spec repository: self packageRepositoryURL ] ]' )).
			         (String lf join:
				          #( '' '{ #category : ''accessing'' }' 'BaselineOfFoo >> packageRepositoryURL ['
				             '^ super packageRepositoryURL ifEmpty: [ ''tonel://elsewhere'' ]'
				             ']' )) }.
	requirements := (self sandboxOnTonel: tonel repository: 'github://o/foo:v2.0.6/src') requirementsInto: MetacelloMVSBaselineReading new.
	self assert: (self descriptionOf: requirements first) equals: 'github://o/foo:v2.0.6/src'
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testAGroupRequiresWhatItReaches [

	| reading |
	reading := MetacelloMVSBaselineReading new.
	(self sandboxOnTonel: (self tonelOf: { (self baselineMethod:
				   #( 'spec for: #common do: [' 'spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ].'
				      'spec baseline: ''Tester'' with: [ spec repository: ''github://o/tester:v1.0.0/src'' ].'
				      'spec package: ''Foo-Core'' with: [ spec requires: #(''Bar'') ].'
				      'spec package: ''Foo-Tests'' with: [ spec requires: #(''Foo-Core'' ''Tester'') ].'
				      'spec group: ''default'' with: #(''Foo-Core'').' 'spec group: ''Tests'' with: #(''Foo-Tests'') ]' )) }))
		requirementsInto: reading.

	self
		assertCollection: ((reading referencesFor: #( 'Tests' ) into: reading) collect: [ :each | self projectOf: each ])
		hasSameElements: #( 'Bar' 'Tester' ).
	self assert: ((reading referencesFor: #(  ) into: reading) collect: [ :each | self projectOf: each ]) asArray equals: #( 'Bar' )
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testAGroupTheBaselineDoesNotDeclareIsAnError [

	| reading found |
	reading := MetacelloMVSBaselineReading new.
	(self sandboxOnTonel: (self tonelOf: { (self baselineMethod: #( 'spec for: #common do: [ spec package: ''Foo-Core'' ]' )) }))
		requirementsInto: reading.
	found := MetacelloMVSBaselineReading new.

	self assert: (reading referencesFor: #( 'Missing' ) into: found) isEmpty.
	self assert: (found findings anySatisfy: [ :each |
			 (each kind = #missingGroup) and: [ each isError and: [ each text includesSubstring: 'Missing' ] ] ]).
	self assert: reading findings isEmpty
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testAPlainBaselineAsksTheImageNothing [

	| sandbox |
	sandbox := self sandboxOnTonel: (self tonelOf: { (self baselineMethod:
				             #( 'spec for: #common do: [ spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ] ]' )) }).
	self
		assert: sandbox imageQueries isEmpty;
		deny: sandbox usesRemovedMetacelloAPI
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testAnAliasKeepsItsNameAndWhatItLoads [
	"Like XMLParser: BitmapCharacterSet, and its alias BitmapCharacterSet Tests loading Tests; both the same project, each kept with how it was declared"

	| requirements |
	requirements := self requirementsOf:
		                #( 'spec for: #common do: [' 'spec baseline: ''Bar'' with: [ spec loads: #(''Core''); repository: ''github://o/bar:v1.2.0/src'' ].'
		                   'spec project: ''Bar Tests'' copyFrom: ''Bar'' with: [ spec loads: #(''Tests'') ].'
		                   'spec package: ''Foo-Core'' with: [ spec requires: #(''Bar'' ''Bar Tests'') ] ]' ).
	self
		assert: (requirements collect: [ :each | self projectOf: each ]) asArray equals: #( 'Bar' 'Bar' );
		assert: (requirements collect: [ :each | each name ]) asSet equals: #( 'Bar' 'Bar Tests' ) asSet;
		assert: (requirements collect: [ :each | each loads ]) asSet
		equals: { #( 'Core' ). #( 'Tests' ) } asSet
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testFindsTheRemovedMetacelloAPI [

	| sandbox |
	sandbox := self sandboxOnTonel: (self tonelOf: { (self baselineMethod:
				             #( 'spec for: #common do: [ spec baseline: ''Core'' with: [ spec repository: self packageRepositoryURL ] ]' )) }).
	self assert: sandbox usesRemovedMetacelloAPI
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testFindsWhatTheBaselineAsksTheImage [
	"Like OrderPreservingDictionary checking Smalltalk globals, NewTools asking BaselineOfPharo"

	| tonel sandbox |
	tonel := self tonelOf: {
			         (self baselineMethod:
				          #( '"Smalltalk in a comment does not count"' 'spec for: #common do: [ spec baseline: ''Bar'' with: [ spec repository: (self classNamed: #BaselineOfPharo) barRepository ] ]' )).
			         (String lf join:
				          #( '' '{ #category : ''accessing'' }' 'BaselineOfFoo >> customProjectAttributes ['
				             '^ (Smalltalk globals includesKey: #GTInspector) ifTrue: [ #( #gt ) ] ifFalse: [ #() ]'
				             ']' )) }.
	sandbox := self sandboxOnTonel: tonel.
	self assert: sandbox imageQueries equals: #( 'Smalltalk' 'classNamed:' )
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testHelperMethodsOfTheBaselineAreCompiledToo [

	| tonel requirements |
	tonel := self tonelOf: {
			         (self baselineMethod: #( 'spec for: #common do: [ self declareBarOn: spec ]' )).
			         (String lf join:
				          #( '' '{ #category : ''baselines'' }' 'BaselineOfFoo >> declareBarOn: spec ['
				             'spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ]'
				             ']' )) }.
	requirements := (self sandboxOnTonel: tonel) requirementsInto: MetacelloMVSBaselineReading new.
	self assert: (requirements collect: [ :each | self projectOf: each ]) asArray equals: #( 'Bar' )
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testInstallsNothing [

	self requirementsOf:
		#( 'spec for: #common do: [ spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ] ]' ).
	self deny: (Smalltalk globals includesKey: #BaselineOfFoo)
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testLeavesAClassOfTheSameNameUntouched [
	"BaselineOfTestToLock2 is in the image: reading another baseline of that name must neither change it nor use it"

	| superclassBefore sourceBefore sandboxClass |
	superclassBefore := BaselineOfTestToLock2 superclass.
	sourceBefore := (BaselineOfTestToLock2 >> #baseline:) sourceCode.
	sandboxClass := (MetacelloMVSBaselineSandbox
		                 onDefinitions: (MetacelloMVSBaselineDefinitions
				                  baselineClassNamed: 'BaselineOfTestToLock2'
				                  method: 'baseline: spec
	<baseline>
	spec for: #common do: [ spec package: ''Other'' ]'
				                  repository: 'github://o/lock2:v1.0.0') definitions
		                 repository: 'github://o/lock2:v1.0.0') baselineClass.

	self deny: sandboxClass identicalTo: BaselineOfTestToLock2.
	self assert: BaselineOfTestToLock2 superclass identicalTo: superclassBefore.
	self assert: (BaselineOfTestToLock2 >> #baseline:) sourceCode equals: sourceBefore.
	self assert: sandboxClass version spec packageNames asArray equals: #( 'Other' )
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testOnlyTheBlocksOfThisPlatformCount [

	| requirements |
	requirements := self requirementsOf:
		                #( 'spec for: #common do: [ spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ] ].'
		                   'spec for: #gemstone do: [ spec baseline: ''Gem'' with: [ spec repository: ''github://o/gem:v1.0.0/src'' ] ].'
		                   'spec for: #pharo do: [ spec baseline: ''Ph'' with: [ spec repository: ''github://o/ph:v1.0.0/src'' ] ]' ).
	self assert: (requirements collect: [ :each | self projectOf: each ]) asSet equals: #( 'Bar' 'Ph' ) asSet
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testProjectsOnlyATestsGroupNeedsAreNotRequired [

	| requirements |
	requirements := self requirementsOf:
		                #( 'spec for: #common do: [' 'spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ].'
		                   'spec baseline: ''Tester'' with: [ spec repository: ''github://o/tester:v1.0.0/src'' ].'
		                   'spec package: ''Foo-Core'' with: [ spec requires: #(''Bar'') ].'
		                   'spec package: ''Foo-Tests'' with: [ spec requires: #(''Foo-Core'' ''Tester'') ].'
		                   'spec group: ''default'' with: #(''Foo-Core'').' 'spec group: ''Tests'' with: #(''Foo-Tests'') ]' ).
	self assert: (requirements collect: [ :each | self projectOf: each ]) asArray equals: #( 'Bar' )
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testReadsTheProjectsABaselineRequires [

	| requirements |
	requirements := self requirementsOf:
		                #( 'spec for: #common do: [' 'spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ].'
		                   'spec baseline: ''Baz'' with: [ spec repository: ''github://o/baz:v2.0.x/src'' ].'
		                   'spec package: ''Foo-Core'' with: [ spec requires: #(''Bar'' ''Baz'') ] ]' ).
	self
		assert: (requirements collect: [ :each | self descriptionOf: each ]) asSet
		equals: #( 'github://o/bar:v1.2.0/src' 'github://o/baz:v2.0.x/src' ) asSet;
		assert: (requirements collect: [ :each | self projectOf: each ]) asSet equals: #( 'Bar' 'Baz' ) asSet
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testTheClassVariablesOfTheBaselineAreDeclared [

	| tonel requirements |
	tonel := String lf join: {
			         'Class {'.
			         '#name : ''BaselineOfFoo'','.
			         '#superclass : ''BaselineOf'','.
			         '#classVars : [ ''Initialized'' ],'.
			         '#instVars : [ ''loaded'' ],'.
			         '#category : ''BaselineOfFoo'''.
			         '}'.
			         (self baselineMethod:
				          #( 'spec for: #common do: [ spec baseline: ''Bar'' with: [ spec repository: ''github://o/bar:v1.2.0/src'' ] ]' )).
			         ''.
			         '{ #category : ''doits'' }'.
			         'BaselineOfFoo >> postLoad ['.
			         'Initialized := true. loaded := true'.
			         ']' }.
	requirements := (self sandboxOnTonel: tonel) requirementsInto: MetacelloMVSBaselineReading new.
	self assert: requirements size equals: 1
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> testTheOldPackageRepositoryURLAnswersTheReadVersionToo [

	| tonel requirements |
	tonel := self tonelOf: { (self baselineMethod:
			          #( 'spec for: #common do: [ spec baseline: ''Core'' with: [ spec repository: self packageRepositoryURL ] ]' )) }.
	requirements := (self sandboxOnTonel: tonel repository: 'github://o/foo:v2.0.6/src') requirementsInto: MetacelloMVSBaselineReading new.
	self assert: (self descriptionOf: requirements first) equals: 'github://o/foo:v2.0.6/src'
]

{ #category : 'tests' }
MetacelloMVSBaselineSandboxTest >> tonelOf: methodSources [

	^ String lf join: { 'Class {'. '#name : ''BaselineOfFoo'','. '#superclass : ''BaselineOf'','. '#category : ''BaselineOfFoo'''.
		  '}' } , methodSources
]
