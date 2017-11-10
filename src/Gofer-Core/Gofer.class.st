"
: Gofer, a person who runs errands. Origin 1960s: from go for, i.e. go and fetch.
: ''The New Oxford American Dictionary''

! Synopsis

Gofer is a small tool on top of Monticello that loads, updates, merges, diffs, reverts, commits, recompiles and unloads groups of Monticello packages. Contrary to existing tools Gofer makes sure that these operations are performed as clean as possible:

- Gofer treats packages from one or more repository in one operation.
- Gofer works with fixed versions or tries to find the ""latest"" version using a given package name.
- Gofer automatically assigns repositories to all packages, so that the other tools are ready to be used on individual packages.
- Gofer makes sure that there is only one repository instance registered for a single physical location.
- Gofer works with Monticello dependencies and uniformly treats them like the primary package.
- Gofer prefers to work with faster repositories if there is a choice.
- Gofer cleans up after Monticello, no empty class categories and no empty method protocols are to be expected.
- Gofer supports operations to sync remote and local repositories with each other.

! Installation

Gofer is included with the latest Pharo and GemStone distributions. To update to the latest version you can use Gofer itself:

== Gofer upgrade

In case you are missing Gofer in your image, grab it from *http://source.lukas-renggli.ch/gofer.html*.

! Description

Gofer is very simple by design, the basic useage scenario is always the same and consists of three steps:

# You specify one or more Monticello repository URLs. You can do this using the methods ==url:==, ==url:username:password:== (HTTP, FTP), ==directory:==, or ==repository:== if you need full control. You might also use the convenience methods like ==squeaksource:==, ==wiresong:==, or ==gemsource:== for well known repositories. Additionally the following settings are available:
#- Gofer implicitly declares the local package cache as a repository. To disable the local package cache use the method ==disablePackageCache==, to re-enable use ==enablePackageCache==.
#- Gofer throws an error if a repository is not reachable. To silently ignore repository erros use the message ==disableRepositoryErrors==, to re-enable use ==enableRepositoryErrors==.
# You specify one or more Monticello packages you want to work with, by adding them to the Gofer instance. Use ==version:== to add a specific version, or use ==package:== to add the ""latest"" version in the given repository. Furthermore there is ==package:constraint:== that allows you to further constraint the version to be loaded in a block passed in as the second argument.
# You specify one or more actions to be performed on the specified packages:

| ==load==	| Load the specified packages.
| ==update==	| Update the specified packages.
| ==merge==	| Merge the specified packages into their working copies.
| ==localChanges==	| Answer the changes between the base version and the working copy.
| ==browseLocalChanges==	| Browse the changes between the base version and the working copy.
| ==remoteChanges==	| Answer the changes between the working copy and the remote changes.
| ==browseRemoteChanges==	| Browse the changes between the working copy and the remote changes.
| ==cleanup==	| Cleans the specified packages.
| ==commit==	| Commit the modified specified packages.
| ==commit:==	| Commit the modified specified packages with the given commit message.
| ==revert==	| Revert the specified packages to the currently loaded version.
| ==recompile==	| Recompile the specified packages.
| ==reinitialize==	| Call the class side initializers on the specified packages.
| ==unload==	| Unload the specified packages.
| ==fetch==     | Download versions from remote repositories into the local cache.
| ==push==      | Upload local versions from local cache into remote repositories.

! Example

To use Gofer to update to exact versions of the Kom Server, the 'latest' code of Seaside 2.8 and the 'latest' code of the Scriptaculous package that is committed by the author with the initials 'lr' one could evaluate:

== Gofer new
==     squeaksource: 'KomHttpServer';
==     version: 'DynamicBindings-gc.7';
==     version: 'KomServices-gc.19';
==     version: 'KomHttpServer-gc.32';
==     update.
== Gofer new
==     squeaksource: 'Seaside';
==     package: 'Seaside2.8a';
==     package: 'Scriptaculous' constraint: [ :version | version author = 'lr' ];
==     load
"
Class {
	#name : #Gofer,
	#superclass : #Object,
	#instVars : [
		'references',
		'repositories',
		'errorBlock',
		'packageCacheRepository',
		'resolvedReferencesCache'
	],
	#category : #Gofer-Core
}

{ #category : #private }
Gofer class >> gofer [
	"Create a Gofer instance of Gofer."

	^ self new
		renggli: 'gofer';
		package: 'Gofer-Core';
		package: 'Gofer-Tests';
		yourself
]

{ #category : #'instance creation' }
Gofer class >> it [
	^ self new
]

{ #category : #deprecated }
Gofer >> addPackage: aPackage [

	self package: aPackage
]

{ #category : #accessing }
Gofer >> allResolved [
	"Answer all sorted references within the configured repositories."
	
	| resolved |
	resolved := OrderedCollection new.
	self repositories
		do: [ :repository | resolved addAll: (self allResolvedIn: repository) ].
	^ resolved asSortedCollection asArray
]

{ #category : #accessing }
Gofer >> allResolvedIn: aRepository [
	"Answer all sorted references within aRepository. For efficiency cache the references."

	^ (resolvedReferencesCache ifNil: [ resolvedReferencesCache := Dictionary new ])
		at: aRepository ifAbsentPut: [ self basicReferencesIn: aRepository ]
]

{ #category : #private }
Gofer >> basicReferencesIn: aRepository [
	^ [ aRepository goferReferences asSortedCollection asArray ] 
		on: GoferRepositoryError
		do: errorBlock
]

{ #category : #repositories-places }
Gofer >> blueplane: aString [
	self url: 'http://squeaksource.blueplane.jp/' , aString
]

{ #category : #operations }
Gofer >> cleanup [
	"Cleans the specified packages."
	
	^ self execute: GoferCleanup
]

{ #category : #operations }
Gofer >> commit [
	"Commit the modified packages."

	^ self execute: GoferCommit
]

{ #category : #operations }
Gofer >> commit: aString [
	"Commit the modified packages with the given commit message."

	^ self execute: GoferCommit do: [ :operation | operation message: aString ]
]

{ #category : #references }
Gofer >> configuration [
	"Add a default Configuration package by using the repository name.
	Assumes that there has been at least one repository set"
	^ self configurationOf: self repositories last project capitalized.
]

{ #category : #references }
Gofer >> configurationOf: aProjectName [
	"Add a package reference to the configuration of the given project"
	references addLast: (GoferConfigurationReference name: aProjectName)
]

{ #category : #repositories }
Gofer >> directory: aDirectoryOrString [
	"Add a file-system repository at aDirectoryOrString."

	| repository |
	repository := (aDirectoryOrString isString and: [ aDirectoryOrString endsWith: '*' ])
		ifTrue: [ 
			MCSubDirectoryRepository new
				directory: aDirectoryOrString allButLast asFileReference;
				yourself ]
		ifFalse: [ 
			(MCDirectoryRepository new)
				directory:
						(aDirectoryOrString isString
								ifTrue: [ aDirectoryOrString asFileReference ]
								ifFalse: [ aDirectoryOrString ]);
				yourself ].
	self repository: repository
]

{ #category : #repositories-options }
Gofer >> disablePackageCache [
	"Disable the use of the package-cache repository."

	packageCacheRepository := nil
]

{ #category : #repositories-options }
Gofer >> disableRepositoryErrors [
	"Silently swallow all repository errors."

	errorBlock := [ :error | error resume: #() ]
]

{ #category : #repositories-options }
Gofer >> enablePackageCache [
	"Enable the use of the package-cache repository."

	packageCacheRepository := MCCacheRepository uniqueInstance.
]

{ #category : #repositories-options }
Gofer >> enableRepositoryErrors [
	"Throw an exception when repositories are not available."

	errorBlock := [ :error | error pass ]
]

{ #category : #private }
Gofer >> execute: anOperationClass [
	^ self execute: anOperationClass do: nil
]

{ #category : #private }
Gofer >> execute: anOperationClass do: aBlock [
	| operation |
	operation := anOperationClass on: self copy.
	aBlock ifNotNil: [ aBlock value: operation ].
	^ operation execute
]

{ #category : #operations }
Gofer >> fetch [
	"Download versions from remote repositories into the local cache."

	^ self execute: GoferFetch
]

{ #category : #repositories-places }
Gofer >> gemsource: aString [
	self url: 'http://seaside.gemstone.com/ss/' , aString
]

{ #category : #repositories-places }
Gofer >> impara: aString [
	self url: 'http://source.impara.de/' , aString
]

{ #category : #initialization }
Gofer >> initialize [
	self enablePackageCache; enableRepositoryErrors.
	references := OrderedCollection new.
	repositories := OrderedCollection new
]

{ #category : #operations }
Gofer >> load [
	"Load the specified packages into the image."
	
	^ self execute: GoferLoad
]

{ #category : #operations }
Gofer >> loadBleedingEdge [
	"Load the baseline of the previously specifed configuration."
	
	^ self loadVersion: #bleedingEdge
]

{ #category : #operations }
Gofer >> loadDevelopment [
	"Load the development version of the previously specifed configuration."
	
	^ self loadVersion: #development
]

{ #category : #operations }
Gofer >> loadStable [
	"Load the stable version of the previously specifed configuration."
	
	^ self loadVersion: #stable
]

{ #category : #operations }
Gofer >> loadVersion: anMetacelloVersionIdentifierSymbol [
	"Load the given version name"
	^ self execute: GoferMetacelloLoad do: [ :operation|
		operation version: anMetacelloVersionIdentifierSymbol ]
]

{ #category : #operations }
Gofer >> localChanges [
	"Answer the changes between the base version and the working copy."
	
	^ self execute: GoferLocalChanges
]

{ #category : #operations }
Gofer >> merge [
	"Merge the specified packages into their working copies."
	
	^ self execute: GoferMerge
]

{ #category : #references }
Gofer >> package: aString [
	"Add the package aString to the receiver. aString is a package name as it appears in the Monticello Browser, something like 'Magritte-Seaside'. Gofer will try to resolve this name to an actual version in one of the configured repositories before doing something with the package. Gofer sorts all the versions in all the repositories according to branch name (versions without a branch are preferred), version number, author name and repository priority. The top hit of this sorted list is eventually going to be loaded and used."

	references addLast: (GoferPackageReference name: aString)
]

{ #category : #references }
Gofer >> package: aString constraint: aOneArgumentBlock [
	"Add the package aString to the receiver, but constraint the resulting versions further with aOneArgumentBlock. For details on the package, see #package:. The following example defines various constraints: 
	
		aGofer package: 'Magritte-Seaside' constraint: [ :version |
			version author = 'lr'
				and: [ version branch = 'trial'
				and: [ version versionNumber > 120 ] ] ]"

	references addLast: (GoferConstraintReference name: aString constraint: aOneArgumentBlock)
]

{ #category : #copying }
Gofer >> postCopy [
	references := references copy.
	repositories := repositories copy.
	resolvedReferencesCache := nil
]

{ #category : #operations }
Gofer >> push [
	"Upload local versions from local cache into remote repositories."

	^ self execute: GoferPush
]

{ #category : #operations }
Gofer >> recompile [
	"Recompile the specified packages."

	^ self execute: GoferRecompile
]

{ #category : #accessing }
Gofer >> references [
	"Answer the configured references."

	^ Array withAll: references
]

{ #category : #operations }
Gofer >> reinitialize [
	"Calls the class side initializers on all package code."
	
	^ self execute: GoferReinitialize
]

{ #category : #operations }
Gofer >> remoteChanges [
	"Display the changes between the working copy and the remote changes."
	
	^ self execute: GoferRemoteChanges
]

{ #category : #repositories-places }
Gofer >> renggli: aString [
	self url: 'http://source.lukas-renggli.ch/' , aString
]

{ #category : #accessing }
Gofer >> repositories [
	"Answer the configured monticello repositories."

	|  result |
	result := OrderedCollection withAll: repositories.
	packageCacheRepository ifNotNil: [ result addFirst: packageCacheRepository ].
	^ result asArray
]

{ #category : #repositories }
Gofer >> repository: aRepository [
	"Add aRepository to the repository configuration. If there is already a repository defined in the global configuration with that URL take this one instead."
	
	| repository |
	repository := MCRepositoryGroup default repositories
		detect: [ :each | each = aRepository ]
		ifNone: [ aRepository ].
	repositories addLast: repository
]

{ #category : #accessing }
Gofer >> resolved [
	"Answer the resolved references of the receiver."

	^ self references collect: [ :each | each resolveWith: self ]
]

{ #category : #operations }
Gofer >> revert [
	"Revert the specified packages to the currently loaded version."
	
	^ self execute: GoferRevert
]

{ #category : #repositories-places }
Gofer >> smalltalkhubUser: aUserName project: aProjectName [
	self repository: (MCSmalltalkhubRepository owner: aUserName project: aProjectName)
]

{ #category : #repositories-places }
Gofer >> squeakfoundation: aString [
	self url: 'http://source.squeakfoundation.org/' , aString
]

{ #category : #repositories-places }
Gofer >> squeaksource3: aProjectName [
	self repository: (MCGemstoneRepository location: 'http://ss3.gemtalksystems.com/ss/' , aProjectName)
]

{ #category : #repositories-places }
Gofer >> squeaksource: aProjectName [
	self repository: (MCSqueaksourceRepository location: 'http://www.squeaksource.com/' , aProjectName)
]

{ #category : #operations }
Gofer >> unload [
	"Unload the specified packages."
	
	^ self execute: GoferUnload
]

{ #category : #operations }
Gofer >> update [
	"Update the specified packages."
	
	^ self execute: GoferUpdate
]

{ #category : #repositories }
Gofer >> url: anUrlString [
	"Add anUrlString as a repository for the following package operations."

	self url: anUrlString username: String new password: String new
]

{ #category : #repositories }
Gofer >> url: urlString username: username password: passwordString [
	"Add urlString as a repository for the following package operations."
	| repository |
	repository := MCRepository fromUrl: 
		(urlString asUrl
			username: (username ifEmpty: [ nil ]);
			password: (passwordString ifEmpty: [ nil ]);
			yourself).
	
	self repository: repository.
]

{ #category : #references }
Gofer >> version: aString [
	"Add the version aString to the receiver. aString is a version name as it appears in the Monticello Repository Browser, something like 'Magritte-Seaside-lr.334'. Gofer will try to resolve this name to one of the configured repositories before loading the code."

	references addLast: (GoferVersionReference name: aString)
]

{ #category : #repositories-places }
Gofer >> wiresong: aString [
	self url: 'http://source.wiresong.ca/' , aString
]
