"
You should use a *baseline* when you are using a disk-based source code manager (SCM) like (http://git-scm.com/).

Your baseline needs a `baseline:` method.
When you change the structure of your project you can change the baseline and save everything in a single commit.

To create a new Metacello baseline:

1. Create a subclass of the **BaselineOf** class. The baseline class for your project should be named by appending the name of your project to the string `BaselineOf`. The name of the category and package should be the same as the name of the class:

```Smalltalk
    BaselineOf << #BaselineOfExample
		slots: {};
		package: 'BaselineOfExample'
```

2. Create a **baseline:** method where you specify the structure of your project:

```Smalltalk
    baseline: spec
      <baseline>

      spec for: #common do: [
        spec
          package: 'Example-Core';
          package: 'Example-Tests' with: [ spec requires: 'Example-Core' ] ]
```

3. Commit your **BaselineOf** package in the repository where your packages are stored

4. To load a package from GitHub that contains a baseline evaluate the following:

```Smalltalk
	Metacello new
		baseline: 'Sample';
		repository: 'github://dalehenrich/metacello-work:master'; ""edit to match your username, repository name and branch""
		load.
```

For further documentation see For more information on the [github://](MetacelloScriptingAPI.md#github) url specifigation see the [Metacello Scripting API reference](MetacelloScriptingAPI.md). There more information on [working with GitHub here](GettingStartedWithGitHub.md).
"
Class {
	#name : 'BaselineOf',
	#superclass : 'Object',
	#instVars : [
		'project'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'accessing' }
BaselineOf class >> allPackageNames [
	
	^ self version packages collect: [:each | each name]
]

{ #category : 'accessing' }
BaselineOf class >> deepPackagesOfGroupNamed: aName [
	"Traverses the group tree to collect packages"
	| allGroups projects group |

	allGroups := self version groups.
	projects := Array streamContents: [ :stream |
		self version spec
			specListProjectDo: [ :each | stream nextPut: each ] 
			packageDo: [ :each | ] 
			groupDo: [  :each | ] ].
	group := allGroups detect: [ :eachGroup | eachGroup name = aName ].
	^ (group includes
		collect: [ :each | 
			(allGroups anySatisfy: [ :eachGroup | eachGroup name = each ])
				ifTrue: [ self deepPackagesOfGroupNamed: each ]
				ifFalse: [
					(projects noneSatisfy: [ :eachProject | eachProject name = each ]) 
						ifTrue: [ { each } ]
						ifFalse: [ #() ] ] ])
		flattened
]

{ #category : 'class initialization' }
BaselineOf class >> initialize [
	"Deprecation added during the development of Pharo 14. Can be removed in a few versions of Pharo."

	self deprecatedAliases: { #ConfigurationOf }
]

{ #category : 'testing' }
BaselineOf class >> isProject [
	"This method can be overriden if we have baselines in the image for tests so that we do not consider it is a project of the image."

	^ true
]

{ #category : 'accessing' }
BaselineOf class >> packagesOfGroupNamed: aName [

	^ (self version groups detect: [ :g | g name = aName ]) includes
]

{ #category : 'accessing' }
BaselineOf class >> project [

	^self new project
]

{ #category : 'unloading Metacello' }
BaselineOf class >> unloadMetacello [
	"Unload the classes that implement Metacello. Metacello is not needed once a project has been loaded, so it can safely be unloaded."

	<script>
	<apiDocumentation>
	(self packageOrganizer packages select: [ :package | package name beginsWith: 'Metacello' ]) do: [ :package | package removeFromSystem ]
]

{ #category : 'accessing' }
BaselineOf class >> version [
	
	^ self project version
]

{ #category : 'accessing' }
BaselineOf class >> withAllPackageNames [
	"Return the name of all packages I includes and the name of my package "

	^ self allPackageNames
		  add: self class package name;
		  yourself
]

{ #category : 'baselines' }
BaselineOf >> baseline: spec [
	<baseline>
	"subclasses should redefine me"
	
	"Here is a typical package and its tests
	spec for: #'common' do: [
		spec 
			package: #'XXX-Core';
			package: #'XXX-Core-Tests' with: [
				spec requires: #('XXX-Core' ) ].
		spec 
			group: 'Core' with: #('XXX-Core' );
			group: 'CoreTests' with: #('XXX-Core' 'XXX-Core-Tests');
			group: 'default' with: #('Beacon-XXX' 'Beacon-XXX-Tests') ]
	"
]

{ #category : 'queries' }
BaselineOf >> classNamed: aSymbol [

	^ self class environment at: aSymbol asSymbol
]

{ #category : 'queries' }
BaselineOf >> classNamed: aSymbol ifPresent: aBlock [

	^ self class environment at: aSymbol asSymbol ifPresent: aBlock
]

{ #category : 'queries' }
BaselineOf >> classNamed: aSymbol ifPresent: aBlock ifAbsent: absentBlock [

	^ self class environment at: aSymbol asSymbol ifPresent: aBlock ifAbsent: absentBlock
]

{ #category : 'accessing' }
BaselineOf >> customProjectAttributes [
    "Edit to return a collection of any custom attributes e.g. for conditional loading: Array with: #'Condition1' with: #'Condition2.
	For more information see: http://code.google.com/p/metacello/wiki/CustomProjectAttrributes "

    ^ #()
]

{ #category : 'accessing' }
BaselineOf >> initalizeProjectWithRepositoryDescription: repositoryDescription [

	^ project := self newProjectWithRepositoryDescription: repositoryDescription
]

{ #category : 'accessing' }
BaselineOf >> newProjectWithRepositoryDescription: aListOrRepositoryDescriptions [

	| aProject |
	aProject := self projectClass new
		            projectAttributes: self customProjectAttributes;
		            setBaselineRepositoryDescription: aListOrRepositoryDescriptions;
		            yourself.
	
	"Construct the project"
	MetacelloBaselineConstructor on: self project: aProject. 
	
	aProject loadType: #linear.

	aListOrRepositoryDescriptions do: [ :desc | aProject version spec repository: desc ].
	^ aProject
]

{ #category : 'accessing' }
BaselineOf >> packageRepositoryURLForSpec: spec [
	" Tries to determine a repository URL from which the baseline is being loaded. Useful for 
	refering other baselines in the same repository. "

	^ spec repositoryDescriptions
		  detect: [ :each | each beginsWith: 'tonel://' ]
		  ifNone: [
			  spec repositoryDescriptions
				  ifEmpty: [ '' ]
				  ifNotEmpty: [ :collection | collection anyOne ] ]
]

{ #category : 'baselines' }
BaselineOf >> pharoVersionsFrom: aVersion [

	^ (aVersion to: SystemVersion current major) collect: [ :v | ('pharo' , v asString , '.x') asSymbol ]
]

{ #category : 'accessing' }
BaselineOf >> project [

	^ project ifNil: [
		  "By default we don't know where the project came from..."
		  self initalizeProjectWithRepositoryDescription: #() ]
]

{ #category : 'accessing' }
BaselineOf >> project: aProject [

	project := aProject
]

{ #category : 'accessing' }
BaselineOf >> projectClass [
    ^ MetacelloProject
]

{ #category : 'queries' }
BaselineOf >> tools [

	^ Smalltalk tools
]
