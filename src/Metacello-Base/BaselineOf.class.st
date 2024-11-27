"
You should use a *baseline* when you are using a disk-based source
code manager (SCM) like [git][1].

When using a disk-based SCM it is not necessary to use the Metacello
*version* method, because it is intended for use with `.mcz` files. 

With a disk-based SCM you only need a single `baseline:` method. When
you change the structure of your project you can change the baseline and
save everything in a single commit. So multiple `baseline:` methods are
no longer needed.

You may still need a *configuration* when using a *baseline*. The
[Sample project][3] on GitHub has a good example of a configuration used
in conjunction with a *baseline*. See the [**ConfigurationOf** class comment][2] 
for information on creating a *configuration*.

To create a new Metacello baseline:

1. Create a subclass of the **BaselineOf** class. The baseline
   class for your project should be named by appending the name of
   your project to the string `BaselineOf`. The name of the category and
   package should be the same as the name of the class:

    ```Smalltalk
    BaselineOf subclass: #BaselineOfExample
      instanceVariableNames: ''
      classVariableNames: ''
      poolDictionaries: ''
      category: 'BaselineOfExample'
    ```

2. Create a **baseline:** method where you specify the structure of your project:

    ```Smalltalk
    baseline: spec
      <baseline>

      spec for: #common do: [
        spec
          package: 'Example-Core';
          package: 'Example-Tests' with: [
            spec requires: 'Example-Core' ]].
    ```

3. Create a Monticello package for your **BaselineOf** class and save it in the repository where your packages are stored.

4. To load a package from GitHub that contains a baseline evaluate the following:

```Smalltalk
| repositorySpec |
""edit to match your username, repository name and branch""
repositorySpec := 'dalehenrich/metacello-work:master'.
Metacello new
  baseline: 'Sample';
  repository: 'github://', repositorySpec;
  load.
```

For further documentation see For more information on the [github://](MetacelloScriptingAPI.md#github) url specifigation see the [Metacello Scripting API
reference](MetacelloScriptingAPI.md). There more information on [working with GitHub here](GettingStartedWithGitHub.md).

[1]: http://git-scm.com/
[2]: https://github.com/dalehenrich/metacello-work/blob/master/repository/Metacello-Base.package/ConfigurationOf.class/README.md
[3]: https://github.com/dalehenrich/sample/tree/configuration/ConfigurationOfSample.package/ConfigurationOfSample.class

"
Class {
	#name : 'BaselineOf',
	#superclass : 'ConfigurationOf',
	#category : 'Metacello-Base',
	#package : 'Metacello-Base'
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
BaselineOf >> newProjectWithRepositoryDescription: aListOrRepositoryDescriptions [

	| newProject |
	newProject := super newProjectWithRepositoryDescription:
		           aListOrRepositoryDescriptions.
	aListOrRepositoryDescriptions do: [ :desc |
		newProject version spec repository: desc ].
	^ newProject
]

{ #category : 'queries' }
BaselineOf >> os [

	^ OSPlatform current
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
BaselineOf >> projectClass [
    ^ MetacelloMCBaselineProject
]

{ #category : 'queries' }
BaselineOf >> tools [

	^ Smalltalk tools
]

{ #category : 'accessing' }
BaselineOf >> versionNumberClass [
    ^ MetacelloVersionNumber
]
