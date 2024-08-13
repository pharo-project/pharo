"
MCWorkingCopy represents one version of a package in memory. It provides support for ancestry access, required packages and the repositories in which the package is managed.
It can know if a package is dirty or not.

The class side manages registered package managers.

Instance Variables:
	versionInfo	<MCAncestry>
	ancestry	<MCWorkingAncestry>
	counter	<Number>
	repositoryGroup	<MCRepositoryGroup>
	requiredPackages	<Collection>
	package	<MCPackage>
	modified	<Boolean>

Class Instance Variables:
	registry	<Dictionary>
"
Class {
	#name : 'MCWorkingCopy',
	#superclass : 'Model',
	#instVars : [
		'ancestry',
		'counter',
		'repositoryGroup',
		'requiredPackages',
		'package',
		'modified'
	],
	#classInstVars : [
		'registry'
	],
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'accessing' }
MCWorkingCopy class >> allWorkingCopies [
	^ self registry values
]

{ #category : 'private' }
MCWorkingCopy class >> ancestorsFromArray: anArray cache: cacheDictionary [
	"While using recursion is simpler, it runs a risk of stack overflow for packages with many ancestors,
	 so we use a local stack to pre-load the cache in reverse order. 
	 The original code remains on the last line, so the intermediate code 
	 does not change the external behavior. "

	| index stack |
	anArray ifNil: [^nil].
	stack := OrderedCollection withAll: anArray.
	index := 1.
	[ index <= stack size ] whileTrue: [
		| dict id |
		dict := stack at: index.
		id := (dict at: #id) asString.
		(cacheDictionary includesKey: id) ifFalse: [
			stack addAll: (dict at: #ancestors ifAbsent: [#()]).
		].
		index := index + 1.
	].
	stack reverseDo: [:each | 
		self infoFromDictionary: each cache: cacheDictionary.
	].
	^ anArray collect: [:dict | self infoFromDictionary: dict cache: cacheDictionary]
]

{ #category : 'accessing' }
MCWorkingCopy class >> ensureForPackage: aPackage [

	^ self registry at: aPackage ifAbsent: [ self registerPackage: aPackage packageOrganizer: self packageOrganizer ]
]

{ #category : 'accessing' }
MCWorkingCopy class >> ensureForPackageNamed: aPackageName [

	^ self ensureForPackageNamed: aPackageName packageOrganizer: self packageOrganizer
]

{ #category : 'accessing' }
MCWorkingCopy class >> ensureForPackageNamed: aPackageName packageOrganizer: aPackageOrganizer [

	^ (self hasPackageNamed: aPackageName)
		  ifTrue: [ self forPackageNamed: aPackageName ]
		  ifFalse: [ self registerPackage: (MCPackage named: aPackageName) packageOrganizer: aPackageOrganizer ]
]

{ #category : 'accessing' }
MCWorkingCopy class >> forPackage: aPackage [

	^ self registry at: aPackage ifAbsent: [ NotFound signalFor: aPackage in: self ]
]

{ #category : 'accessing' }
MCWorkingCopy class >> forPackageNamed: aPackageName [

	^ self registry values
		  detect: [ :workingCopy | workingCopy packageName = aPackageName ]
		  ifNone: [ NotFound signalFor: aPackageName in: self ]
]

{ #category : 'system changes' }
MCWorkingCopy class >> handleClassAndMethodsChange: anEvent [

	anEvent packagesAffected do: [ :package | package mcWorkingCopy ifNotNil: [ :workingCopy | workingCopy modified: true ] ]
]

{ #category : 'querying' }
MCWorkingCopy class >> hasPackageNamed: aName [

	^ self allWorkingCopies anySatisfy: [ :each | each packageName = aName ]
]

{ #category : 'private' }
MCWorkingCopy class >> infoFromDictionary: aDictionary cache: cache [
	| id |
	id := (aDictionary at: #id) asString.
	^ cache at: id ifAbsentPut:
		[MCVersionInfo
			name: (aDictionary at: #name ifAbsent: [''])
			id: (UUID fromString: id)
			message: (aDictionary at: #message ifAbsent: [''])
			date: ([Date readFrom: (aDictionary at: #date) pattern: 'yyyy-mm-dd' ] onErrorDo: [nil])
			time: ([Time fromString: (aDictionary at: #time)] onErrorDo: [nil])
			ancestors: (self ancestorsFromArray: (aDictionary at: #ancestors ifAbsent: []) cache: cache)
			stepChildren: (self ancestorsFromArray: (aDictionary at: #stepChildren ifAbsent: []) cache: cache)]
]

{ #category : 'class initialization' }
MCWorkingCopy class >> initialize [

	registry ifNotNil: [ registry rehash ].
	self registerForNotifications
]

{ #category : 'system changes' }
MCWorkingCopy class >> packageAdded: anAnnouncement [

	^ self ensureForPackageNamed: anAnnouncement package name packageOrganizer: anAnnouncement package organizer
]

{ #category : 'system changes' }
MCWorkingCopy class >> packageRemoved: anAnnouncement [

	anAnnouncement package mcWorkingCopy ifNotNil: [ :wc | wc unregister ]
]

{ #category : 'system changes' }
MCWorkingCopy class >> packageRenamed: anAnnouncement [

	| oldWorkingCopy |
	oldWorkingCopy := self forPackageNamed: anAnnouncement oldName.

	"Let's make sure we have a working copy for this new package and unregister the old one."
	(self packageAdded: anAnnouncement)
		modified: true;
		repositoryGroup: oldWorkingCopy repositoryGroup.

	oldWorkingCopy unload
]

{ #category : 'event registration' }
MCWorkingCopy class >> registerForNotifications [

	<systemEventRegistration>
	self
		unregisterForNotifications;
		registerInterestOnSystemChanges
]

{ #category : 'event registration' }
MCWorkingCopy class >> registerInterestOnSystemChanges [

	self class codeChangeAnnouncer weak
		when: PackageAdded send: #packageAdded: to: self;
		when: PackageRenamed send: #packageRenamed: to: self;
		when: PackageRemoved send: #packageRemoved: to: self;
		when: ClassAnnouncement , MethodAnnouncement send: #handleClassAndMethodsChange: to: self
]

{ #category : 'private' }
MCWorkingCopy class >> registerPackage: aPackage packageOrganizer: aPackageOrganizer [

	| workingCopy |
	aPackage environment: aPackageOrganizer environment.
	workingCopy := self new initializeWithPackage: aPackage.
	self registry at: aPackage put: workingCopy.
	"When creating a MC package and a working copy, we need to ensure we create a system package also in case someone creates a package without an associated system package.
	But we still check that the package does not exist because the working copy creation might be caused by the creation of the system package and we do not want to end up in a loop."
	aPackageOrganizer packageNamed: aPackage name ifAbsent: [ aPackageOrganizer ensurePackage: aPackage name ].
	^ workingCopy
]

{ #category : 'accessing' }
MCWorkingCopy class >> registry [
	^ registry ifNil: [registry := Dictionary new]
]

{ #category : 'event registration' }
MCWorkingCopy class >> unregisterForNotifications [

	self codeChangeAnnouncer unsubscribe: self
]

{ #category : 'operations' }
MCWorkingCopy >> adopt: aVersion [
	ancestry addAncestor: aVersion info.
	self changed.
]

{ #category : 'accessing' }
MCWorkingCopy >> allAncestors [
	^ self versionInfo 
		ifNotNil: [ :versionInfo | versionInfo allAncestors ]
		ifNil: [ #() ]
]

{ #category : 'accessing' }
MCWorkingCopy >> ancestors [
	^ ancestry ancestors
]

{ #category : 'accessing' }
MCWorkingCopy >> ancestry [
	^ ancestry
]

{ #category : 'operations' }
MCWorkingCopy >> clearRequiredPackages [
	requiredPackages := nil
]

{ #category : 'operations' }
MCWorkingCopy >> collectDependenciesWithMessage: messageString in: aRepository [
	
	^ self requiredPackages collect: [:aPackage | 
			MCVersionDependency
				package: aPackage
				info:  (aPackage workingCopy 
					currentVersionInfoWithMessage: messageString 
					in: aRepository) ]
	
]

{ #category : 'accessing' }
MCWorkingCopy >> completeSnapshot [
	"return a complete snapshot of the loaded sources in this working copy. 
	unlike snapshot this includes also the snapshots of all packages"
	
	| definitions |
	
	definitions := OrderedCollection withAll: package snapshot definitions.
	
	self requiredPackages 
		do: [ :aPackage| definitions addAll: aPackage workingCopy completeSnapshot definitions ]
		displayingProgress: [ :item| 'Loading dependencies from: ', item name ].
	
	^ MCSnapshot fromDefinitions: definitions
]

{ #category : 'accessing' }
MCWorkingCopy >> currentVersionInfoWithMessage: aMessageString in: aRepository [
	^ (self needsSaving or: [ancestry ancestors isEmpty])
		ifTrue: [ (self newVersionWithMessage: aMessageString in: aRepository) info ]
		ifFalse: [ancestry ancestors first]
]

{ #category : 'displaying' }
MCWorkingCopy >> displayStringOn: stream [

	ancestry ancestors
		ifEmpty: [
			stream
				nextPutAll: self package name;
				nextPutAll: ' (unsaved)' ]
		ifNotEmpty: [ :ancestors |
			ancestors first displayStringOn: stream.
			self needsSaving ifTrue: [ stream nextPutAll: ' (modified)' ] ]
]

{ #category : 'private' }
MCWorkingCopy >> findSnapshotWithVersionInfo: aVersionInfo [
    "when an ancestor inside the ancestor chain is not found, does not pass nil instead.
    With this change we can now browse history and delta between them without having to 
    have the complete history"
    "instead of asking for the user to add a new repository, or copy the the missing package we simply
    return an empty Snapshot by returning nil"

    ^ aVersionInfo
        ifNil: [MCSnapshot empty]
        ifNotNil: [(self repositoryGroup versionWithInfo: aVersionInfo ifNone: [nil])
            ifNil: [MCSnapshot empty]
            ifNotNil: [:aVersion | aVersion snapshot]]
]

{ #category : 'initialization' }
MCWorkingCopy >> initialize [
	super initialize.
	modified := false.
	ancestry := MCWorkingAncestry new
]

{ #category : 'initialization' }
MCWorkingCopy >> initializeWithPackage: aPackage [
	package := aPackage.
	self initialize.
]

{ #category : 'operations' }
MCWorkingCopy >> loaded: aVersion [
	ancestry := MCWorkingAncestry new addAncestor: aVersion info.
	requiredPackages := OrderedCollection withAll: (aVersion dependencies collect: [:ea | ea package]).
	self modified: false.
	self changed
]

{ #category : 'operations' }
MCWorkingCopy >> merged: aVersion [
	ancestry addAncestor: aVersion info.
	aVersion dependencies do: [:ea | self requirePackage: ea package].
	self changed
]

{ #category : 'accessing' }
MCWorkingCopy >> modified [
	^ modified
]

{ #category : 'accessing' }
MCWorkingCopy >> modified: aBoolean [

	modified = aBoolean ifTrue: [ ^ self ].
	modified := aBoolean.
	self changed: #modified.
	modified ifFalse: [ ChangesLog default logChange: '"' , self packageName , '"' ]
]

{ #category : 'accessing' }
MCWorkingCopy >> needsSaving [
	^ self modified or: [self requiredPackages anySatisfy: [:ea | ea workingCopy needsSaving]]
]

{ #category : 'operations' }
MCWorkingCopy >> newVersionWithMessage: aMessageString in: aRepository [

	^ self newVersionWithName: (self uniqueVersionNameIn: aRepository) message: aMessageString in: aRepository
]

{ #category : 'operations' }
MCWorkingCopy >> newVersionWithName: nameString message: messageString in: aRepository [
	
	| info deps |
	info := ancestry infoWithName: nameString message: messageString.
	ancestry := MCWorkingAncestry new addAncestor: info.
	self modified: true; modified: false.
	
	deps := self collectDependenciesWithMessage: messageString in: aRepository.
	
	(self repositoryGroup includes: aRepository) 
		ifFalse: [ self repositoryGroup addRepository: aRepository ].

	^ MCVersion
		package: package
		info: info
		snapshot: package snapshot
		dependencies: deps
]

{ #category : 'accessing' }
MCWorkingCopy >> nextAncestors [
	^ self versionInfo 
		ifNotNil: [ :versionInfo | versionInfo ancestors ]
		ifNil: [ #() ]
]

{ #category : 'private' }
MCWorkingCopy >> nextVersionName [

	| branch oldName base |
	branch := ''.
	ancestry ancestors
		ifEmpty: [
			counter ifNil: [ counter := 0 ].
			base := package name ]
		ifNotEmpty: [ :ancestors |
			oldName := ancestors first name.
			oldName last isDigit
				ifFalse: [ base := oldName ]
				ifTrue: [
					base := oldName copyUpToLast: $-.
					branch := ((oldName copyAfterLast: $-) copyUpToLast: $.) copyAfter: $. ].
			counter ifNil: [
				counter := (ancestors collect: [ :each |
					            each name last isDigit
						            ifFalse: [ 0 ]
						            ifTrue: [ ('0' , (each name copyAfterLast: $.) select: [ :ea | ea isDigit ]) asNumber ] ]) max ] ].

	branch ifNotEmpty: [ branch := '.' , branch ].
	counter := counter + 1.
	^ base , '-' , branch , '.' , counter asString
]

{ #category : 'accessing' }
MCWorkingCopy >> package [
	^ package
]

{ #category : 'accessing' }
MCWorkingCopy >> packageName [
	^ package name
]

{ #category : 'printing' }
MCWorkingCopy >> printOn: aStream [

	super printOn: aStream.
	package name ifNotNil: [ aStream 
										nextPutAll: '(' ; 
										print: package name ;
 										nextPut: $)]
]

{ #category : 'accessing' }
MCWorkingCopy >> removeRequiredPackage: aPackage [

	requiredPackages remove: aPackage ifAbsent: []

]

{ #category : 'repositories' }
MCWorkingCopy >> repositoryGroup [
	^ repositoryGroup ifNil: [repositoryGroup := MCRepositoryGroup new]
]

{ #category : 'repositories' }
MCWorkingCopy >> repositoryGroup: aRepositoryGroup [
	repositoryGroup := aRepositoryGroup
]

{ #category : 'accessing' }
MCWorkingCopy >> requirePackage: aPackage [
	(self requiredPackages includes: aPackage) ifFalse: [requiredPackages add: aPackage]
]

{ #category : 'accessing' }
MCWorkingCopy >> requiredPackages [
	^ requiredPackages ifNil: [requiredPackages := OrderedCollection new]
]

{ #category : 'accessing' }
MCWorkingCopy >> snapshot [
	^ self package snapshot
]

{ #category : 'accessing' }
MCWorkingCopy >> systemPackage [

	^ self package systemPackage
]

{ #category : 'private' }
MCWorkingCopy >> uniqueVersionNameIn: aRepository [
	|versionName|
	counter := nil.
	'Creating unique version number' 
		displayProgressFrom: 0 
		to: 1 
		during: [ :arg|
			[versionName := self nextVersionName.
			aRepository includesVersionNamed: versionName] whileTrue ].
	^ versionName
]

{ #category : 'operations' }
MCWorkingCopy >> unload [
	"Unloads mcpackage, rpackage, classes and method extensions from this working copy"

	| postUnloadAction |
	postUnloadAction := [  ].
	self systemPackage ifNotNil: [ :aPackage |
		aPackage packageManifestOrNil ifNotNil: [ :manifest |
			postUnloadAction := manifest postUnloadAction.
			manifest preUnload ] ].
	MCPackageLoader unloadPackage: self package.
	self systemPackage ifNotNil: [ :aPackage | aPackage removeFromSystem ].
	self unregister.
	postUnloadAction value
]

{ #category : 'operations' }
MCWorkingCopy >> unregister [

	self class registry removeKey: package ifAbsent: [ ^ self ].
	self class changed: { #unregistered . package }
]

{ #category : 'accessing' }
MCWorkingCopy >> versionInfo [
	^ self ancestors 
		ifNotEmpty: [ :list | list first ] 
		ifEmpty: [ nil ]
]

{ #category : 'accessing' }
MCWorkingCopy >> versionInfo: aVersionInfo [
	ancestry := MCWorkingAncestry new addAncestor: aVersionInfo
]
