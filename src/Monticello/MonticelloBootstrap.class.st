"
This class contains the required behavior to bootstrap monticello in a new image. A new image has no monticello meta-data, working copies, or whatsoever. Then, Monticello (this package) is initially loaded using an ST file in chunk format (see the CodeImporter package). Once Monticello is installed, this class is required to:

 - recreate the corresponding MC working copies for each package in the system
 - reload all methods, recreate their source code (and thus create the .changes file)

I can be created from a directory using the #inDirectory: class message. e.g.,

  MonticelloBootstrap inDirectory: 'test'.

Then, the method #loadPackageVersion: will load a specific version, and #loadBootstrapPackages will load the packages in the correct order for the corresponding version of the bootstrap.

The convenience method #bootstrapDefault will create an instance with the default parameters for bootstrap and load all bootstrap packages.

  MonticelloBootstrap bootstrapDefault
"
Class {
	#name : 'MonticelloBootstrap',
	#superclass : 'Object',
	#instVars : [
		'directory',
		'localRepository'
	],
	#category : 'Monticello-Base',
	#package : 'Monticello',
	#tag : 'Base'
}

{ #category : 'instance creation' }
MonticelloBootstrap class >> bootstrapDefault [

	(self inDirectory: self defaultDirectory)
		bootstrap
]

{ #category : 'defaults' }
MonticelloBootstrap class >> defaultDirectory [

	^ FileLocator localDirectory / 'package-cache'
]

{ #category : 'instance creation' }
MonticelloBootstrap class >> inDirectory: aDirectory [

	^ self new
		directory: aDirectory;
		yourself
]

{ #category : 'bootstrapping' }
MonticelloBootstrap >> bootstrap [

	self createWorkingCopies.
	self loadBootstrapPackages.

	SessionManager default registerSystemClassNamed: #SmalltalkImage
]

{ #category : 'versions' }
MonticelloBootstrap >> bootstrapVersionsExcept: aBlock [

	^ self bootstrapVersionsSatisfying: [ :arg | (aBlock value: arg) not ]
]

{ #category : 'versions' }
MonticelloBootstrap >> bootstrapVersionsSatisfying: aBlock [

	^ PackageOrganizer default packages
		  select: [ :package | package name ~= UndefinedPackage undefinedPackageName and: [ aBlock value: package ] ]
		  thenCollect: [ :rpackage | self fileNameForPackageNamed: rpackage name ]
]

{ #category : 'bootstrapping' }
MonticelloBootstrap >> createWorkingCopies [
	"For each of the packages inside the image, excepted by the Unpackaged package, create a monticello working copy.
	This is achieved by creating an instance of a working copy and snapshotting it. This generates and registers the working copy automatically."

	(PackageOrganizer default packageNames copyWithout: UndefinedPackage undefinedPackageName) asSortedCollection do: [ :packageName |
		SystemNotification signal: 'Creating Snapshot of: ' , packageName.
		(MCWorkingCopy ensureForPackageNamed: packageName) snapshot ]
]

{ #category : 'accessing' }
MonticelloBootstrap >> directory: aDirectory [

	directory := aDirectory
]

{ #category : 'versions' }
MonticelloBootstrap >> fileNameForPackageNamed: name [

	| versionInfos |
	versionInfos := self localRepository directory files
		select: [ :fileName | (fileName basename copyUpToLast: $-) = name ].
	
	versionInfos
		ifEmpty: [ self error: 'No versionInfo for package: ', name,' in ', self localRepository directory asString ].
	versionInfos size > 1
		ifTrue: [ self error: 'More than one versionInfo for package "', name , '": ', versionInfos asArray asString ].
	
	^ versionInfos anyOne basename
]

{ #category : 'bootstrapping' }
MonticelloBootstrap >> loadBootstrapPackageNamed: aName [

	(self bootstrapVersionsSatisfying: [ :package | aName = package name ]) do: [ :version | self loadPackageVersion: version ]
]

{ #category : 'bootstrapping' }
MonticelloBootstrap >> loadBootstrapPackages [

	MCCacheRepository cacheDirectory: directory.

	(self bootstrapVersionsExcept: [ :package | #( 'Shift-ClassBuilder' 'Monticello' ) includes: package name ]) do: [ :version | self loadPackageVersion: version ].

	"We load ShiftClassBuilder at the end because it has a patch to work in the bootstrap process. The patch is used when there is no source."

	self loadBootstrapPackageNamed: 'Shift-ClassBuilder'.

	"We load lastly monticello because we are using it to reload itself.
	Othewise, we should handle the case that after loading monticello we should return to a safe place from the stack where no old methods or classes are used."
	self loadBootstrapPackageNamed: 'Monticello'
]

{ #category : 'bootstrapping' }
MonticelloBootstrap >> loadPackageNamed: aName [

	self loadPackagesNamed: { aName }
]

{ #category : 'bootstrapping' }
MonticelloBootstrap >> loadPackageVersion: aFileName [

	| previous |
	"Until the bootstrap of Pharo runs on Pharo 12, the MCZ exported will be exported with categories in MCZ format instead of packages and tags. Using #readingOldExport should allow to bootstrap until we run the bootstrap in Pharo 12."
	previous := MCMczReader readingOldExport.
	[
	MCMczReader readingOldExport: true.

	[
	SystemNotification signal: 'Loading ' , aFileName asString.
	"Use snapshot install instead of load to force loading. Otherwise monticello finds no changes and does not load anything."
	(self localRepository loadVersionFromFileNamed: aFileName) load ]
		on: MCMergeOrLoadWarning
		do: [ :warning |
			SystemNotification signal: 'Warning: ' , warning messageText asString.
			warning resume: true ] ] ensure: [ MCMczReader readingOldExport: previous ]
]

{ #category : 'bootstrapping' }
MonticelloBootstrap >> loadPackagesNamed: aCollectionOfNames [

	aCollectionOfNames do: [ :name | | filename |
		filename := self fileNameForPackageNamed: name.
		self loadPackageVersion: filename ]
]

{ #category : 'accessing' }
MonticelloBootstrap >> localRepository [

	^ localRepository ifNil: [
		localRepository := MCDirectoryRepository new
			directory: directory asFileReference;
			yourself.	]
]
