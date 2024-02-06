"
# Metacello User Guide

In this guide we'll take a walk through a couple of common development
scenarios and highlight some of the features of the *Metacello Scripting
API*.

*For installatation and more detailed documentation on the Metacello
Scripting API, see the [Metcello Scripting API Documentation][1].*

## Introduction

The number one job of the *Metacello Scripting API* is to simplify the
job of loading projects into your image. As you are probably all
too aware, today it's a two step process where you first load the
configuration into your image using [Gofer][2] and then load your
project using Metacello:

```Smalltalk
Gofer new
  package: 'ConfigurationOfSeaside30';
  squeaksource: 'MetacelloRepository';
  load.
((Smalltalk at: #ConfigurationOfSeaside30) version: #stable) load.
```

In the early days of Metacello (and Gofer) this was a great improvement
over the alternatives, but today, years after the introduction of
Metacello, there should be a better way...and there is.
Using the *Metacello Scripting API* the above expression reduces to the
following:

```Smalltalk
Metacello new
	repository: 'github://SeasideSt/Seaside:v3.4.6/repository';
	baseline: 'Seaside';
	load.
```

## Loading

In this example of the [`load` command][5] we are leveraging a couple of
default values, namely the `version` of the project and the `repository` where the
**BaselineOfSeaside** package can be found:

```Smalltalk
Metacello new
	repository: 'github://SeasideSt/Seaside:v3.4.6/repository';
	baseline: 'Seaside';
	load.
```

You can also specify the full [repository description][3] as follows:

```Smalltalk
Metacello new
	baseline: 'Seaside';
	repository: 'github://SeasideSt/Seaside:v3.4.6/repository';
  	load.
```

##Listing

Once you've loaded one or more projects into your image, you may want to
list them. The following is an example of the [`list` command][6]:

```Smalltalk
Metacello image
	baseline: [ :spec | true ];
	list.
```

The `image` message tells Metacello that you'd like to look
at only loaded baselines. 

The *block* argument to the
`baseline:` message is used to *select* against the list of loaded
[MetacelloProjectSpec][7] instances in the [registry][8].

The `list` command itself returns a list of [MetacelloProjectSpec][7] instances that can be printed, inspected or otherwise manipulated.

In addition to a *select block*, you can specify a *select collection*
specifying the names of the projects you'd like to select:

```Smalltalk
Metacello registry
	baseline: #('Seaside' 'MetacelloPreview');
	list.
```

The `registry` message tells Metacello that you'd like to
look at all projects in the [registry][8] whether or not they are loaded.

The *collection* argument to the `baseline:` message is used to
*select* against the list of project names in the [registry][8].

## Getting

Once you've loaded a project into your image the next logical step is
upgrading your project to a new version. 

Let's say that a new `#stable` version of Seaside has been released
and that you want to upgrade. This is a two step process: 

* [get a new version of the configuration][11]
* [load the new version][12]

### Get a new version of the configuration

The following expression gets the latest version of the
baseline:

```Smalltalk
Metacello image
	baseline: 'Seaside';
	get.
```

By using the `image` message, you can leverage the fact that the [registry][8] remembers
from which repository you loaded the original version of the baseline.

The `get` command simply downloads the latest version of the
configuration package from the repository.

You may download the configuration from a different repository:

```Smalltalk
Metacello image
	baseline: 'Seaside30';
	repository: 'github://path/to/repository';
	get.
```

The `get` command will update the [registry][8] with the new
repository location information.

You may also use the `get` command to load a baseline for a project
into your image without actually loading the project itself:

```Smalltalk
Metacello image
	baseline: 'SeasideRest';
	repository: 'github://SeasideSt/Seaside:v3.4.6/repository';
	get.
```

The 'SeasideRest' project information will be registered in the [registry][8] and marked
as *unloaded*.

### Load the new version

Once you've got a new copy of the Seaside30 configuration loaded into your image, you may
upgrade your image with the following expression:

```Smalltalk
Metacello image
	baseline: 'Seaside';
	load.
```

By using the `image` message, you are asking Metacello to look the
project up in the [registry][8] before performing the
operation, so it isn't necessary to supply all of the project details for every
command operation.

Of course, the `load` command updates the [registry][8].

In this case you use the `registry` message to indicate that you are
interested in both *loaded* and *unloaded* projects.

##Locking

Let's say that you are using an older version of Seaside (say 3.0.5)
instead of the #stable version (3.0.7) and that your application doesn't
work with newer versions of Seaside (you've tried and it's more work
to get you application to work with the newer version of Seaside than
it's worth).

Let's also say that you want to try out something in the
SeasideRest project, but when you try loading SeasideRest, you end up
having Seaside 3.0.7 loaded as well. 

This is an unfortunate side effect of Metacello trying to *do the right
thing*, only in your case it is the wrong thing.

Fortunately, the [`lock` command][9] can give you control. First you
need to `lock` the Seaside project:

```Smalltalk
Metacello image
  baseline: 'Seaside';
  lock.
```

The `image` message tells Metacello to do a lookup in the list of loaded
projects and then to put a lock on the loaded version of the project.

After a project is locked, a project trying to load it will just skip it.
Notice that you are responsible for any problem product of this lock.

## Unlocking

You can unlock your project by executing:

```Smalltalk
Metacello image
  baseline: 'Seaside';
  unlock.
```

[1]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md
[2]: http://www.lukas-renggli.ch/blog/gofer
[3]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#repository-descriptions
[4]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#repository-shortcuts
[5]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#loading
[6]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#listing
[7]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#metacelloprojectspec
[8]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#metacello-project-registry
[9]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#locking
[10]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloScriptingAPI.md#metacello-version-numbers
[11]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloUserGuide.md#get-a-new-version-of-the-configuration
[12]: https://github.com/dalehenrich/metacello-work/blob/master/docs/MetacelloUserGuide.md#load-the-new-version
"
Class {
	#name : 'Metacello',
	#superclass : 'Object',
	#instVars : [
		'statements',
		'scriptExecutorClass'
	],
	#category : 'Metacello-Base',
	#package : 'Metacello-Base'
}

{ #category : 'instance creation' }
Metacello class >> image [

	^ self new
		  scriptExecutorClass: MetacelloScriptImageExecutor;
		  yourself
]

{ #category : 'instance creation' }
Metacello class >> registrations [
  ^ MetacelloProjectRegistration registry registrations
]

{ #category : 'instance creation' }
Metacello class >> registry [

	^ self new
		  scriptExecutorClass: MetacelloScriptRegistryExecutor;
		  yourself
]

{ #category : 'private' }
Metacello class >> scriptExecutorClass [

	^ MetacelloScriptApiExecutor
]

{ #category : 'private' }
Metacello >> addStatement: selector args: args [
    self statements add: selector -> args
]

{ #category : 'api projectSpec' }
Metacello >> baseline: projectName [
    self addStatement: #'baselineArg:' args: {projectName}
]

{ #category : 'api repository shortcuts' }
Metacello >> bitbucketUser: userName project: projectName commitish: commitish path: path [

	self repository: 'bitbucket://' , userName , '/' , projectName , ':' , (commitish ifEmpty: [ 'master' ]) , '/' , path
]

{ #category : 'api projectSpec' }
Metacello >> className: className [
    self addStatement: #'classNameArg:' args: {className}
]

{ #category : 'api projectSpec' }
Metacello >> configuration: projectName [
    self addStatement: #'configurationArg:' args: {projectName}
]

{ #category : 'private' }
Metacello >> execute: selector args: args [
  | script |
  script := self statements copy.
  script add: selector -> args.
  ^ self scriptExecutor execute: script
]

{ #category : 'api actions' }
Metacello >> fetch [
  ^ self execute: #'fetch:' args: #(#())
]

{ #category : 'api actions' }
Metacello >> fetch: required [
  ^ self execute: #'fetch:' args: {required}
]

{ #category : 'api repository shortcuts' }
Metacello >> filetreeDirectory: directoryName [
  self repository: 'filetree://' , directoryName
]

{ #category : 'api repository shortcuts' }
Metacello >> gemsource: projectName [
  self repository: 'http://seaside.gemtalksystems.com/ss/' , projectName
]

{ #category : 'api actions' }
Metacello >> get [
  "resolve project name in given repository and return an instance of MetacelloProject resolved from a ConfigurationOf or BaselineOf"

  ^ self execute: #'get' args: #()
]

{ #category : 'api repository shortcuts' }
Metacello >> githubUser: userName project: projectName commitish: commitish path: path [

	self repository: 'github://' , userName , '/' , projectName , ':' , (commitish ifEmpty: [ 'master' ]) , '/' , path
]

{ #category : 'api options' }
Metacello >> ignoreImage [
    "ignore image state"

    self addStatement: #'ignoreImage:' args: {true}
]

{ #category : 'api actions' }
Metacello >> list [
  "list projects in registry"

  ^ self execute: #'list' args: #()
]

{ #category : 'api actions' }
Metacello >> load [
  ^ self execute: #'load:' args: #(#())
]

{ #category : 'api actions' }
Metacello >> load: required [
  ^ self execute: #'load:' args: {required}
]

{ #category : 'api options' }
Metacello >> loader: aLoader [

	self addStatement: #loader: args: { aLoader }
]

{ #category : 'api actions' }
Metacello >> lock [
  "lock projects in registry"

  ^ self execute: #'lock' args: #()
]

{ #category : 'api actions' }
Metacello >> locked [
  "list of locked projects in registry"

  ^ self
    project: [ :projectSpec | projectSpec isLocked ];
    list
]

{ #category : 'api options' }
Metacello >> onConflict: aBlock [
    self addStatement: #'onConflict:' args: {aBlock}
]

{ #category : 'api options' }
Metacello >> onConflictUseIncoming [
  self onConflict: [ :ex :loaded :incoming | ex useIncoming ]
]

{ #category : 'api options' }
Metacello >> onConflictUseIncoming: incomingProjects useLoaded: loadedProjects [
  self
    onConflict: [ :ex :loaded :incoming | 
      (incomingProjects includes: incoming baseName)
        ifTrue: [ ex useIncoming ]
        ifFalse: [ 
          (loadedProjects includes: incoming baseName)
            ifTrue: [ ex useLoaded ] ].
      ex pass ]
]

{ #category : 'api options' }
Metacello >> onConflictUseLoaded [
  self onConflict: [ :ex :loaded :incoming | ex useLoaded ]
]

{ #category : 'api options' }
Metacello >> onDowngrade: aBlock [
    self addStatement: #'onDowngrade:' args: {aBlock}
]

{ #category : 'api options' }
Metacello >> onDowngradeUseIncoming [
  self onDowngrade: [ :ex :loaded :incoming | ex useIncoming ]
]

{ #category : 'api options' }
Metacello >> onDowngradeUseIncoming: projectNames [
  self
    onDowngrade: [ :ex :loaded :incoming | 
      (projectNames includes: loaded baseName)
        ifTrue: [ ex useIncoming ]
        ifFalse: [ ex useLoaded ] ]
]

{ #category : 'api options' }
Metacello >> onLock: aBlock [
  self addStatement: #'onLock:' args: {aBlock}
]

{ #category : 'api options' }
Metacello >> onLockBreak [
  self onLock: [ :ex :loaded :incoming | ex break ]
]

{ #category : 'api options' }
Metacello >> onLockBreak: projectNames [
  self
    onLock: [ :ex :loaded :incoming | 
      (projectNames includes: loaded baseName)
        ifTrue: [ ex break ]
        ifFalse: [ ex honor ] ]
]

{ #category : 'api options' }
Metacello >> onUpgrade: aBlock [
    self addStatement: #'onUpgrade:' args: {aBlock}
]

{ #category : 'api options' }
Metacello >> onUpgradeUseLoaded [
  self onUpgrade: [ :ex :loaded :incoming | ex useLoaded ]
]

{ #category : 'api options' }
Metacello >> onUpgradeUseLoaded: projectNames [
  self
    onUpgrade: [ :ex :loaded :incoming | 
      (projectNames includes: loaded baseName)
        ifTrue: [ ex useLoaded ]
        ifFalse: [ ex useIncoming ] ]
]

{ #category : 'api options' }
Metacello >> onWarning: aBlock [
  self addStatement: #'onWarning:' args: {aBlock}
]

{ #category : 'api options' }
Metacello >> onWarningLog [
  self
    onWarning: [ :ex | 
      MetacelloNotification signal: ex description.
      ex resume ]
]

{ #category : 'api projectSpec' }
Metacello >> project: projectName [
    self addStatement: #'projectArg:' args: {projectName}
]

{ #category : 'api actions' }
Metacello >> record [
  ^ self execute: #'record:' args: #(#())
]

{ #category : 'api actions' }
Metacello >> record: required [
  ^ self execute: #'record:' args: {required}
]

{ #category : 'api actions' }
Metacello >> register [
  "change registered project"

  ^ self execute: #'register' args: #()
]

{ #category : 'api projectSpec' }
Metacello >> repository: repositoryDescription [
    self addStatement: #'repositoryArg:' args: {repositoryDescription}
]

{ #category : 'accessing' }
Metacello >> scriptExecutor [

	^ self scriptExecutorClass new
]

{ #category : 'accessing' }
Metacello >> scriptExecutorClass [

	^ scriptExecutorClass ifNil: [ scriptExecutorClass := MetacelloScriptApiExecutor ]
]

{ #category : 'accessing' }
Metacello >> scriptExecutorClass: anObject [

	scriptExecutorClass := anObject
]

{ #category : 'api options' }
Metacello >> silently [
    "no progress bars"

    self addStatement: #'silently:' args: {true}
]

{ #category : 'api repository shortcuts' }
Metacello >> smalltalkhubUser: userName project: projectName [

	self repository: 'http://smalltalkhub.com/mc/' , userName , '/' , projectName , '/main'
]

{ #category : 'api repository shortcuts' }
Metacello >> squeaksource3: projectName [
  self repository: 'http://ss3.gemtalksystems.com/ss/' , projectName
]

{ #category : 'api repository shortcuts' }
Metacello >> squeaksource: projectName [
    self repository: 'http://www.squeaksource.com/' , projectName
]

{ #category : 'api repository shortcuts' }
Metacello >> ss3: projectName [
    self squeaksource3: projectName
]

{ #category : 'accessing' }
Metacello >> statements [

	^ statements ifNil: [ statements := OrderedCollection new ]
]

{ #category : 'accessing' }
Metacello >> statements: anObject [
	statements := anObject
]

{ #category : 'api actions' }
Metacello >> unlock [
  "unlock projects in registry"

  ^ self execute: #'unlock' args: #()
]

{ #category : 'api actions' }
Metacello >> unregister [
  "unlock projects in registry"

  ^ self execute: #'unregister' args: #()
]

{ #category : 'api projectSpec' }
Metacello >> version: versionString [
    self addStatement: #'versionArg:' args: {versionString}
]
