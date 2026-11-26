"
I check the version spec that `MetacelloBaselineConstructor` builds from a realistic baseline, not from an empty one: my own `baseline:` method is the baseline of Metacello itself (packages, the `Core`, `Tests` and `default` groups), extended with dependencies on other projects and with platform specific declarations.

My tests read the result through the access API of `MetacelloVersionSpec`: `packageNamed:`, `packages`, `groups`, `projects`, `packageNames` and `defaultPackageNames`. They are sorted by subject: packages, groups, dependent projects and `for:do:` attributes.

## Design Rationale

- I am my own baseline, as `MetacelloAbstractSpecTest` is: `MetacelloBaselineConstructor class>>on:` performs the method holding the `<baseline>` pragma on the object it receives, so no fixture `BaselineOf` class is needed.
- The baseline declares three kinds of dependent projects: one with `baseline:with:` (`Clap`, default class name), one with `project:with:` and an explicit `className:` (`IcebergLibraries`), and one with `project:copyFrom:with:` (`ClapTests`).
- It has a block for `#pharo`, an attribute of the current platform, which adds a package and extends the `Core` group, and a block for `#gemstone`, which must be ignored.
- `setUp` evaluates the baseline once per test; the version spec is kept in `version`.
"
Class {
	#name : 'MetacelloBaselineConstructorTest',
	#superclass : 'MetacelloTestCase',
	#instVars : [
		'version'
	],
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
MetacelloBaselineConstructorTest >> baseline: spec [
	"The baseline of Metacello itself, extended with dependencies on other projects and with platform specific declarations"

	<baseline>
	spec for: #common do: [
		self declareProjectsIn: spec.
		self declarePackagesIn: spec.
		self declareGroupsIn: spec ].
	spec for: #pharo do: [
		spec
			package: 'Metacello-Pharo' with: [ spec requires: #( 'Metacello-Core' ) ];
			group: 'Core' with: #( 'Metacello-Pharo' ) ].
	spec for: #gemstone do: [
		spec
			package: 'Metacello-GemStone';
			group: 'Core' with: #( 'Metacello-GemStone' ) ]
]

{ #category : 'baselines' }
MetacelloBaselineConstructorTest >> declareGroupsIn: spec [

	spec
		group: 'Core' with: #( 'Metacello-Core' 'Metacello-CommandLine' 'Metacello-MVS' );
		group: 'Tests' with: #( 'Metacello-Tests' 'Metacello-MVS-Tests' );
		group: 'default' with: #( 'Core' )
]

{ #category : 'baselines' }
MetacelloBaselineConstructorTest >> declarePackagesIn: spec [

	spec
		package: 'Metacello-Core';
		package: 'Metacello-CommandLine' with: [ spec requires: #( 'Metacello-Core' 'Clap' ) ];
		package: 'Metacello-MVS' with: [ spec requires: #( 'Metacello-Core' 'IcebergLibraries' ) ];
		package: 'Metacello-Tests' with: [ spec requires: #( 'Metacello-Core' ) ];
		package: 'Metacello-MVS-Tests' with: [ spec requires: #( 'Metacello-MVS' 'Metacello-Tests' 'ClapTests' ) ]
]

{ #category : 'baselines' }
MetacelloBaselineConstructorTest >> declareProjectsIn: spec [

	spec
		baseline: 'Clap' with: [
			spec
				repository: 'github://pharo-contributions/clap-st:v1.0.0/src';
				loads: #( 'Clap-Core' ) ];
		project: 'IcebergLibraries' with: [
			spec
				className: 'BaselineOfIceberg';
				repository: 'github://pharo-vcs/iceberg:v2.4.0';
				loads: #( 'Iceberg-Libgit' ) ];
		project: 'ClapTests' copyFrom: 'Clap' with: [ spec loads: #( 'Clap-Tests' ) ]
]

{ #category : 'helpers' }
MetacelloBaselineConstructorTest >> namesOf: aCollectionOfSpecs [

	^ aCollectionOfSpecs collect: [ :each | each name ] as: Array
]

{ #category : 'helpers' }
MetacelloBaselineConstructorTest >> projectNamed: aString [
	"Answer the project spec referenced by the dependency named aString"

	^ (version packageNamed: aString) referencedSpec
]

{ #category : 'running' }
MetacelloBaselineConstructorTest >> setUp [

	super setUp.
	version := (MetacelloBaselineConstructor on: self) project version
]

{ #category : 'tests - groups' }
MetacelloBaselineConstructorTest >> testAllGroupIncludesEveryDeclaredName [

	| all |
	all := version packageNamed: 'ALL'.
	self assert: all isGroupSpec.
	self assert: all includes asArray equals: version packageNames asArray
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testBaselineProjectGetsDefaultClassName [

	self assert: (self projectNamed: 'Clap') className equals: 'BaselineOfClap'
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testBaselineProjectLoads [

	self assert: (self projectNamed: 'Clap') loads equals: #( 'Clap-Core' )
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testBaselineProjectRepository [

	| repository |
	repository := (self projectNamed: 'Clap') repositorySpecs first.
	self assert: (self projectNamed: 'Clap') repositorySpecs size equals: 1.
	self assert: repository description equals: 'github://pharo-contributions/clap-st:v1.0.0/src'.
	self assert: repository type equals: 'github'
]

{ #category : 'tests - attributes' }
MetacelloBaselineConstructorTest >> testBlockForAnotherPlatformIsIgnored [

	self assert: (version packageNamed: 'Metacello-GemStone') isNil.
	self deny: ((version packageNamed: 'Core') includes includes: 'Metacello-GemStone')
]

{ #category : 'tests - attributes' }
MetacelloBaselineConstructorTest >> testBlockForCurrentPlatformDeclaresItsPackage [

	self assert: (version packageNamed: 'Metacello-Pharo') requires equals: #( 'Metacello-Core' )
]

{ #category : 'tests - attributes' }
MetacelloBaselineConstructorTest >> testBlockForCurrentPlatformIsMergedInGroup [

	self
		assert: (version packageNamed: 'Core') includes
		equals: #( 'Metacello-Core' 'Metacello-CommandLine' 'Metacello-MVS' 'Metacello-Pharo' )
]

{ #category : 'tests - groups' }
MetacelloBaselineConstructorTest >> testDefaultPackageNamesIsTheDefaultGroup [

	self assert: version defaultPackageNames equals: #( 'default' )
]

{ #category : 'tests - groups' }
MetacelloBaselineConstructorTest >> testGroupIncludesGroup [

	self assert: (version packageNamed: 'default') includes equals: #( 'Core' ).
	self assert: (version packageNamed: 'Core') isGroupSpec
]

{ #category : 'tests - groups' }
MetacelloBaselineConstructorTest >> testGroupIncludesPackages [

	| group |
	group := version packageNamed: 'Tests'.
	self assert: group isGroupSpec.
	self assert: group includes equals: #( 'Metacello-Tests' 'Metacello-MVS-Tests' )
]

{ #category : 'tests - groups' }
MetacelloBaselineConstructorTest >> testGroups [

	self assert: (self namesOf: version groups) asSet equals: #( 'Core' 'Tests' 'default' ) asSet
]

{ #category : 'tests - packages' }
MetacelloBaselineConstructorTest >> testPackageNamesListsProjectsPackagesAndGroups [

	self
		assert: version packageNames asArray
		equals: #( 'Clap' 'IcebergLibraries' 'ClapTests' 'Metacello-Core' 'Metacello-CommandLine' 'Metacello-MVS' 'Metacello-Tests'
			   'Metacello-MVS-Tests' 'Core' 'Tests' 'default' 'Metacello-Pharo' )
]

{ #category : 'tests - packages' }
MetacelloBaselineConstructorTest >> testPackageRequiresPackages [

	self assert: (version packageNamed: 'Metacello-MVS-Tests') requires equals: #( 'Metacello-MVS' 'Metacello-Tests'
	   'ClapTests' )
]

{ #category : 'tests - packages' }
MetacelloBaselineConstructorTest >> testPackageRequiresProject [

	| package |
	package := version packageNamed: 'Metacello-CommandLine'.
	self assert: package requires equals: #( 'Metacello-Core' 'Clap' ).
	self assert: (version packageNamed: 'Clap') isProjectReferenceSpec
]

{ #category : 'tests - packages' }
MetacelloBaselineConstructorTest >> testPackageWithoutRequirements [

	| package |
	package := version packageNamed: 'Metacello-Core'.
	self assert: package isPackageSpec.
	self assert: package name equals: 'Metacello-Core'.
	self assert: package requires isEmpty.
	self assert: package includes isEmpty
]

{ #category : 'tests - packages' }
MetacelloBaselineConstructorTest >> testPackages [

	self
		assert: (self namesOf: version packages)
		equals: #( 'Metacello-Core' 'Metacello-CommandLine' 'Metacello-MVS' 'Metacello-Tests' 'Metacello-MVS-Tests'
		   'Metacello-Pharo' )
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testProjectCopiedFromAnotherProjectHasItsOwnLoads [

	self assert: (self projectNamed: 'ClapTests') loads equals: #( 'Clap-Tests' ).
	self assert: (self projectNamed: 'Clap') loads equals: #( 'Clap-Core' )
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testProjectCopiedFromAnotherProjectKeepsItsClassNameAndRepository [

	| project |
	project := self projectNamed: 'ClapTests'.
	self assert: project name equals: 'ClapTests'.
	self assert: project className equals: 'BaselineOfClap'.
	self assert: project repositoryDescriptions asArray equals: #( 'github://pharo-contributions/clap-st:v1.0.0/src' )
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testProjectReferenceAnswersTheProject [

	| reference |
	reference := version packageNamed: 'Clap'.
	self assert: reference isProjectReferenceSpec.
	self assert: reference projectName equals: 'Clap'.
	self assert: reference referencedSpec identicalTo: (version projects detect: [ :each | each name = 'Clap' ])
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testProjectWithExplicitClassName [

	| project |
	project := self projectNamed: 'IcebergLibraries'.
	self assert: project name equals: 'IcebergLibraries'.
	self assert: project className equals: 'BaselineOfIceberg'.
	self assert: project loads equals: #( 'Iceberg-Libgit' ).
	self assert: project repositoryDescriptions asArray equals: #( 'github://pharo-vcs/iceberg:v2.4.0' )
]

{ #category : 'tests - projects' }
MetacelloBaselineConstructorTest >> testProjects [

	self assert: (self namesOf: version projects) equals: #( 'Clap' 'IcebergLibraries' 'ClapTests' )
]

{ #category : 'tests - packages' }
MetacelloBaselineConstructorTest >> testUnknownPackageNameAnswersNil [

	self assert: (version packageNamed: 'Metacello-Unknown') isNil
]

{ #category : 'tests - packages' }
MetacelloBaselineConstructorTest >> testUnknownPackageNameEvaluatesAbsentBlock [

	self assert: (version packageNamed: 'Metacello-Unknown' ifAbsent: [ #absent ]) equals: #absent
]
