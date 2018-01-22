"
You must use a *configuration* when your project is stored in a repository using `.mcz` files.

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
	#name : #ConfigurationOf,
	#superclass : #Object,
	#instVars : [
		'project'
	],
	#category : #'Metacello-Base'
}

{ #category : #private }
ConfigurationOf class >> bootstrapMetacelloFrom: repositoryUrl [
  "Corresponds to version 1.0.0-beta.32.6"

  "KEEP MetacelloConfigTemplate class>>ensureMetacelloBaseConfiguration in synch!"

  | platformPkg |
  self ensureGoferVersion: 'Gofer-Core-lr.115' repositoryUrl: repositoryUrl.
  #('Metacello-Core-dkh.678' 'Metacello-MC-dkh.674')
    do: [ :pkg | self bootstrapPackage: pkg from: repositoryUrl ].
  platformPkg := Smalltalk
    at: #'SystemVersion'
    ifPresent: [ :cl | 
      | versionString |
      versionString := cl current version.
      (versionString beginsWith: 'Squeak')
        ifTrue: [ 
          (versionString beginsWith: 'Squeak3')
            ifTrue: [ 'Metacello-Platform.squeak-dkh.5' ]
            ifFalse: [ 'Metacello-Platform.squeak-dkh.22' ] ]
        ifFalse: [ 
          (versionString beginsWith: 'Pharo')
            ifTrue: [ 
              self bootstrapPackage: 'Metacello-PharoCommonPlatform-dkh.2' from: repositoryUrl.
              (versionString beginsWith: 'Pharo2')
                ifTrue: [ 'Metacello-Platform.pharo20-dkh.33' ]
                ifFalse: [ 'Metacello-Platform.pharo-dkh.34' ] ] ] ].
  self bootstrapPackage: platformPkg from: repositoryUrl
]

{ #category : #private }
ConfigurationOf class >> bootstrapPackage: aString from: aPath [
    | repository version |
    repository := (MCCacheRepository uniqueInstance includesVersionNamed: aString)
        ifTrue: [ MCCacheRepository uniqueInstance ]
        ifFalse: [ MCHttpRepository location: aPath user: '' password: '' ].
    self
        retry: [ 
            repository
                versionReaderForFileNamed: aString , '.mcz'
                do: [ :reader | 
                    version := reader version.
                    version load.
                    version workingCopy repositoryGroup addRepository: repository ] ]
]

{ #category : #private }
ConfigurationOf class >> ensureGoferVersion: goferVersion repositoryUrl: repositoryUrl [
    "load the p=file goferVersion if Gofer isn't loaded or an earlier version of Gofer is currently loaded"

    | goferVersionNumber wc pName |
    (Smalltalk at: #'Gofer' ifAbsent: [  ]) == nil
        ifTrue: [ ^ self bootstrapPackage: goferVersion from: repositoryUrl ].
    goferVersionNumber := (goferVersion copyAfterLast: $.) asNumber.
    wc := [ ((Smalltalk at: #'GoferPackageReference') name: 'Gofer') workingCopy ]
        on: Error
        do: [ :ex | ex return: ((Smalltalk at: #'GoferPackageReference') name: 'Gofer-Core') workingCopy ].
    pName := wc ancestry ancestors first name.
    (pName copyAfterLast: $.) asNumber <= goferVersionNumber
        ifTrue: [ self bootstrapPackage: goferVersion from: repositoryUrl ]
]

{ #category : #private }
ConfigurationOf class >> ensureMetacello [
    "Bootstrap Metacello and load the 'botstrap' group"

    self ensureMetacello: #('batch')
]

{ #category : #private }
ConfigurationOf class >> ensureMetacello: loadList [
  "Bootstrap Metacello, retry using alternate repository, if primary repository is not accessible"

  Smalltalk
    at: #'MetacelloProject'
    ifAbsent: [ 
      | version error gofer |
      (Array
        with: 'http://smalltalkhub.com/mc/dkh/metacello/main'
        with: 'http://seaside.gemtalksystems.com/ss/metacello')
        do: [ :repositoryUrl | 
          "bootstrap Metacello"
          [ 
          self bootstrapMetacelloFrom: repositoryUrl.
          Smalltalk
            at: #'ConfigurationOfMetacello'
            ifAbsent: [ 
              self
                retry: [ 
                  gofer := (Smalltalk at: #'Gofer') new.
                  gofer
                    perform: #'url:' with: repositoryUrl;
                    perform: #'package:' with: 'ConfigurationOfMetacello';
                    perform: #'load' ] ].
          version := (Smalltalk at: #'ConfigurationOfMetacello') project
            version: #'previewBootstrap'.
          version load: loadList.
          self
            retry: [ 
              gofer := (Smalltalk at: #'Gofer') new.
              Smalltalk
                at: #'ConfigurationOfMetacelloPreview'
                ifAbsent: [ 
                  gofer
                    perform: #'url:' with: repositoryUrl;
                    perform: #'package:'
                      with: 'ConfigurationOfMetacelloPreview';
                    perform: #'load' ] ].
          version := (Smalltalk at: #'ConfigurationOfMetacelloPreview') project
            version: #'stable'.	"load latest from GitHub"
          version load: loadList.
          ^ self ]
            on: Error
            do: [ :ex | 
              error := ex.
              Transcript
                cr;
                show: 'failed ensureMetacello using ';
                show: repositoryUrl printString;
                show: ' : ';
                show: ex description printString;
                show: '...retrying'.	"try again"
              ex return: nil ] ].	"shouldn't get here unless the load failed ... throw an error"
      self
        error:
          'retry with alternate repository failed: ' , error description printString ]
]

{ #category : #accessing }
ConfigurationOf class >> project [

	^self new project
]

{ #category : #private }
ConfigurationOf class >> retry: aBlock [
    self retry: aBlock retryCount: 3
]

{ #category : #private }
ConfigurationOf class >> retry: aBlock retryCount: retryCount [
    | count |
    count := 1.
    [ true ]
        whileTrue: [ 
            [ 
            aBlock value.
            ^ self ]
                on: Error
                do: [ :ex | 
                    count < retryCount
                        ifTrue: [ 
                            Transcript
                                cr;
                                show: 'RETRYING AFTER:';
                                cr;
                                show: ex description printString.
                            (Delay forSeconds: 5) wait ]
                        ifFalse: [ 
                            Transcript
                                cr;
                                show: 'FAILED RETRYING:';
                                cr;
                                show: ex description printString.
                            ex pass ] ].
            count := count + 1 ]
]

{ #category : #'unloading Metacello' }
ConfigurationOf class >> unloadMetacello [
	"Unload the classes that implement Metacello. Metacello is not needed once a project has been loaded, so it can safely be unloaded."

	"self unloadMetacello"

	<apiDocumentation>
	| gofer |
	gofer := (Smalltalk at: #Gofer) new.
	MCWorkingCopy allManagers do: [:wc |
		((wc packageName beginsWith: 'Metacello') or: [ wc packageName beginsWith: 'OB-Metacello' ])
			ifTrue: [ gofer package: wc packageName ]].
	gofer unload.
]

{ #category : #accessing }
ConfigurationOf class >> validate [
	"Check the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). 
	Errors identify specification issues that will result in unexpected behaviour when you load the configuration. 
	Critical Warnings identify specification issues that may result in unexpected behavior when you load the configuration.
	Warnings identify specification issues that are technically correct, but are worth take a look at."

	"self validate"

	<apiDocumentation>
	self ensureMetacello.
	^ ((Smalltalk at: #MetacelloToolBox) validateConfiguration: self debug: #() recurse: false) inspect
]

{ #category : #defaults }
ConfigurationOf >> bleedingEdge [ 
	"override if different behavior desired.
	 Use:
		self versionDoesNotExistError: #bleedingEdge
	 if #bleedingEdge version is disallowed."

	<defaultSymbolicVersion: #bleedingEdge>
	
	^self defaultBleedingEdgeVersion
]

{ #category : #accessing }
ConfigurationOf >> customProjectAttributes [
    "Edit to return a collection of any custom attributes e.g. for conditional loading: Array with: #'Condition1' with: #'Condition2.
	For more information see: http://code.google.com/p/metacello/wiki/CustomProjectAttrributes "

    ^ #()
]

{ #category : #defaults }
ConfigurationOf >> defaultBleedingEdgeVersion [
	| bleedingEdgeVersion |
	bleedingEdgeVersion := (self project map values select: [ :version | version blessing == #baseline ])
		detectMax: [ :version | version ].
	bleedingEdgeVersion ifNil: [ ^#'notDefined' ].
	^ bleedingEdgeVersion versionString
]

{ #category : #accessing }
ConfigurationOf >> project [
    ^ project
        ifNil: [ 
            "Bootstrap Metacello if it is not already loaded"
            self class ensureMetacello.
            project := self projectClass new projectAttributes: self customProjectAttributes.	"Create the Metacello project"
            project versionNumberClass: self versionNumberClass.
            project class versionConstructorClass on: self project: project.	"Construct the project"
            project loadType: #'linear'.	"change to #atomic if desired"
            project ]
]

{ #category : #accessing }
ConfigurationOf >> project: aProject [

	project ifNil: [ self class ensureMetacello ].
	project := aProject
]

{ #category : #accessing }
ConfigurationOf >> projectClass [
    ^ MetacelloMCProject
]

{ #category : #private }
ConfigurationOf >> versionDoesNotExistError: versionStringOrSymbol [

	((Smalltalk at: #MetacelloSymbolicVersionDoesNotExistError) project: self project versionString: versionStringOrSymbol) signal
]

{ #category : #accessing }
ConfigurationOf >> versionNumberClass [
    ^ MetacelloSemanticVersionNumber
]
