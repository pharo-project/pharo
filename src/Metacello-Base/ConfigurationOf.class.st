"
You must use a *configuration* when your project is stored in a
repository using `.mcz` files.

If you are using a source code manager (SCM) like [git][1] and have
created a *baseline* (see the [**BaselineOf** class comment][3] for more info)
you may use a *configuration* to associate a specific
git commit (SHA, branch name, tag name) with a [Metacello version][2].

To create a new Metacello configuration:

1. Create a subclass of the **ConfigurationOf** class. The configuration
   class for your project should be names by appending the name of
   your project to the string `ConfigurationOf`. The name of the
   category and package should be the same as the name of the class:

    ```Smalltalk
    ConfigurationOf subclass: #ConfigurationOfExample
      instanceVariableNames: ''
      classVariableNames: ''
      poolDictionaries: ''
      category: 'ConfigurationOfExample'
    ```

2. Create a **baselineXXXX:** method where you specify the structure of your project:

    ```Smalltalk
    baseline0100: spec
      <baseline: '1.0-baseline'>

      spec for: #common do: [
        spec repository: 'http://ss3.gemstone.com/ss/Example'.
        spec
          package: 'Example-Core';
          package: 'Example-Tests' with: [
            spec requires: 'Example-Core' ]].
    ```

3. Create a **versionXXXX:** method where you specify the specific
   versions of the packages to be loaded for this version:

    ```Smalltalk
    version01000: spec
      <version: '1.0' imports: #('1.0-baseline')>

      spec for: #common do: [
        spec blessing: #release.
        spec
          package: 'Example-Core' with: 'Example-Core';
          package: 'Example-Tests' with: 'Example-Tests' ].
    ```

4. Create a Monticello package for your **ConfigurationOf** class and save it in the repository where your packages are stored. 

[1]: http://git-scm.com/
[2]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#metacello-version-numbers
[3]: https://github.com/dalehenrich/metacello-work/blob/master/repository/Metacello-Base.package/BaselineOf.class/README.md

"
Class {
	#name : 'ConfigurationOf',
	#superclass : 'Object',
	#instVars : [
		'project'
	],
	#category : 'Metacello-Base',
	#package : 'Metacello-Base'
}

{ #category : 'accessing' }
ConfigurationOf class >> project [

	^self new project
]

{ #category : 'private' }
ConfigurationOf class >> retry: aBlock [
    self retry: aBlock retryCount: 3
]

{ #category : 'private' }
ConfigurationOf class >> retry: aBlock retryCount: retryCount [
	| count |
	count := 1.
	[ true ]
		whileTrue: [ [ aBlock value.
			^ self ]
				on: Error
				do: [ :ex | 
					count < retryCount
						ifTrue: [ MetacelloNotification signal: 'RETRYING AFTER: ', String cr, ex description.
							(Delay forSeconds: 5) wait ]
						ifFalse: [ MetacelloNotification signal: 'FAILED RETRYING:', String cr, ex description.
							ex pass ] ].
			count := count + 1 ]
]

{ #category : 'unloading Metacello' }
ConfigurationOf class >> unloadMetacello [
	"Unload the classes that implement Metacello. Metacello is not needed once a project has been loaded, so it can safely be unloaded."

	"self unloadMetacello"

	<apiDocumentation>
	| gofer |
	gofer := Gofer new.
	MCWorkingCopy allWorkingCopies do: [ :wc |
		((wc packageName beginsWith: 'Metacello') or: [ wc packageName beginsWith: 'OB-Metacello' ]) ifTrue: [ gofer package: wc packageName ] ].
	gofer unload
]

{ #category : 'accessing' }
ConfigurationOf class >> validate [
	"Check the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). 
	Errors identify specification issues that will result in unexpected behaviour when you load the configuration. 
	Critical Warnings identify specification issues that may result in unexpected behavior when you load the configuration.
	Warnings identify specification issues that are technically correct, but are worth take a look at."

	"self validate"

	<apiDocumentation>
	^ ((self environment at: #MetacelloToolBox) validateConfiguration: self debug: #(  ) recurse: false) inspect
]

{ #category : 'defaults' }
ConfigurationOf >> bleedingEdge [ 
	"override if different behavior desired.
	 Use:
		self versionDoesNotExistError: #bleedingEdge
	 if #bleedingEdge version is disallowed."

	<defaultSymbolicVersion: #bleedingEdge>
	
	^self defaultBleedingEdgeVersion
]

{ #category : 'accessing' }
ConfigurationOf >> customProjectAttributes [
    "Edit to return a collection of any custom attributes e.g. for conditional loading: Array with: #'Condition1' with: #'Condition2.
	For more information see: http://code.google.com/p/metacello/wiki/CustomProjectAttrributes "

    ^ #()
]

{ #category : 'defaults' }
ConfigurationOf >> defaultBleedingEdgeVersion [

	| bleedingEdgeVersion |
	bleedingEdgeVersion := (self project map values select: [ :version | version blessing == #baseline ]) detectMax: [ :version | version ].
	bleedingEdgeVersion ifNil: [ ^ #notDefined ].
	^ bleedingEdgeVersion versionString
]

{ #category : 'accessing' }
ConfigurationOf >> project [

	^ project ifNil: [
		  project := self projectClass new projectAttributes: self customProjectAttributes. "Create the Metacello project"
		  project versionNumberClass: self versionNumberClass.
		  project class versionConstructorClass on: self project: project. "Construct the project"
		  project loadType: #linear. "change to #atomic if desired"
		  project ]
]

{ #category : 'accessing' }
ConfigurationOf >> project: aProject [

	project := aProject
]

{ #category : 'accessing' }
ConfigurationOf >> projectClass [
    ^ MetacelloMCProject
]

{ #category : 'private' }
ConfigurationOf >> versionDoesNotExistError: versionStringOrSymbol [

	(MetacelloSymbolicVersionDoesNotExistError project: self project versionString: versionStringOrSymbol) signal
]

{ #category : 'accessing' }
ConfigurationOf >> versionNumberClass [
    ^ MetacelloSemanticVersionNumber
]
