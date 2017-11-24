"
A MCVersionLoader is responsible for loading a given version. 
"
Class {
	#name : #MCVersionLoader,
	#superclass : #Object,
	#instVars : [
		'versions'
	],
	#category : #'Monticello-Loading'
}

{ #category : #'public api' }
MCVersionLoader class >> loadVersion: aVersion [
	self new
		addVersion: aVersion;
		load
]

{ #category : #loading }
MCVersionLoader >> addDependency: aDependency [
	| dep |
	aDependency isCurrent ifTrue: [^ self].
	(self depAgeIsOk: aDependency) ifFalse: [^ self].
	dep := aDependency resolve.
	dep
		ifNil: [self confirmMissingDependency: aDependency]
		ifNotNil: [(versions includes: dep) ifFalse: [self addVersion: dep]]
]

{ #category : #loading }
MCVersionLoader >> addVersion: aVersion [
	aVersion dependencies do: [ :ea | self addDependency: ea].
	versions add: aVersion.

]

{ #category : #private }
MCVersionLoader >> announceLoad: aString do: aBlock [
	| returnValue |
	returnValue := nil.
	[ self announceLoadStart: aString ] ensure: [ 
		[ returnValue := aBlock value ] ensure: [ 
			self announceLoadStop: aString ] ].
	^ returnValue
]

{ #category : #private }
MCVersionLoader >> announceLoadStart: aString [
	SystemAnnouncer uniqueInstance announce: (MCVersionLoaderStarted new
		versionLoader: self;
		label: aString; 
		yourself)
]

{ #category : #private }
MCVersionLoader >> announceLoadStop: aString [
	SystemAnnouncer uniqueInstance announce: (MCVersionLoaderStopped new
		versionLoader: self;
		label: aString; 
		yourself)
]

{ #category : #private }
MCVersionLoader >> basicLoadWithNameLike: aString [
	| loader |
	
	self checkForModificationsIfCancel: [ ^ self] ifMerge: [ ^ self mergeVersions ].
	
	loader := versions size > 1
		ifTrue: [MCMultiPackageLoader new]
		ifFalse: [MCPackageLoader new].
		
	versions do: [:ea |
		ea canOptimizeLoading
			ifTrue: [ea patch applyTo: loader]
			ifFalse: [loader updatePackage: ea package withSnapshot: ea snapshot]].
		
	loader loadWithNameLike: aString.
	versions do: [:ea | ea workingCopy loaded: ea]
]

{ #category : #checking }
MCVersionLoader >> checkForModificationsIfCancel: cancelBlock ifMerge: mergeBlock [
	| modifications |
	
	"first check if there are local packages which are dirty"
	modifications := self localModifications.
	modifications isEmpty ifTrue: [ ^ self].
	
	"ask the user what to do.. merge | overwrite | abort"
	^ self warnAboutLosingChangesTo: modifications ifCancel: cancelBlock ifMerge: mergeBlock
]

{ #category : #checking }
MCVersionLoader >> checkIfDepIsOlder: aDependency [
	^ aDependency isOlder not 
		or: [self confirm: 'load older dependency ', aDependency versionInfo name , '?']
]

{ #category : #checking }
MCVersionLoader >> confirmMissingDependency: aDependency [
	| name |
	name := aDependency versionInfo name.
	(self confirm: 'Can''t find dependency ', name, '. ignore?')
		ifFalse: [self error: 'Can''t find dependency ', name]
]

{ #category : #checking }
MCVersionLoader >> depAgeIsOk: aDependency [
	^ aDependency isOlder not 
		or: [self confirm: 'load older dependency ', aDependency versionInfo name , '?']
]

{ #category : #private }
MCVersionLoader >> ensurePackage: mcPackage [ 
	RPackageOrganizer default registerPackageNamed: mcPackage name 
		
]

{ #category : #testing }
MCVersionLoader >> hasLocalModifications [
	^ self localModifications isEmpty not
	
]

{ #category : #testing }
MCVersionLoader >> hasVersions [

	^ versions isEmpty not
]

{ #category : #initialization }
MCVersionLoader >> initialize [
	super initialize.
	versions := OrderedCollection new
]

{ #category : #loading }
MCVersionLoader >> load [
	RPackageSet withCacheDo: [   
		| version |
		version := versions first. 
		[ self ensurePackage: version package.
		  self loadWithNameLike: version info name ] asJob
				title: 'Loading ', version info name asString;
				run ]
]

{ #category : #loading }
MCVersionLoader >> loadWithNameLike: aString [
	" this is THE central entrypoint for all loading (gopher, metacello, monticello, ...) "
	^ self announceLoad: aString do: [ self basicLoadWithNameLike: aString ]
]

{ #category : #checking }
MCVersionLoader >> localModifications [
	^ versions select: [:ea | ea package workingCopy modified].
	
]

{ #category : #loading }
MCVersionLoader >> mergeVersions [
	|merger|
	merger :=  MCVersionMerger new.
	merger addVersions: self localModifications.
	merger merge.
]

{ #category : #checking }
MCVersionLoader >> warnAboutLosingChangesTo: versionCollection [
	^ (MCMergeOrLoadWarning signalFor: versionCollection).
]

{ #category : #checking }
MCVersionLoader >> warnAboutLosingChangesTo: versionCollection ifCancel: cancelBlock ifMerge: mergeBlock [
	
	| result |

	result := (MCMergeOrLoadWarning signalFor: versionCollection).
	
	result = false ifTrue: [ ^ mergeBlock value ].
	result = nil ifTrue: [ ^ cancelBlock value ].
	
	"otherwise we will continue loading"
]
