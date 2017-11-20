"
A MBAbstractInfoList is an abstraction used to hold information on groups, packages and projects.

Instance Variables
	groups:		a list of groups (MBGroupInfo instances)
	infoList:		<Object>
	packages:		<Object>
	projects:		<Object>

"
Class {
	#name : #MBAbstractInfoList,
	#superclass : #MBInfo,
	#instVars : [
		'packages',
		'projects',
		'groups',
		'infoList'
	],
	#category : #Versionner-Core-Model
}

{ #category : #private }
MBAbstractInfoList >> calculateInfoList [

	^self subclassResponsibility 
]

{ #category : #updating }
MBAbstractInfoList >> fullRecalculate [

	self recalculate
]

{ #category : #accessing-computed }
MBAbstractInfoList >> groups [
	"Return the list of groups for the configuration"

	| groupSpecs |
	groups ~~ nil
		ifTrue: [ ^ groups ].
	groups := OrderedCollection new.
	self versionForInfoList ifNil: [ ^ groups ].
	groupSpecs := self versionForInfoList groups.
	groupSpecs := groupSpecs asSortedCollection: [ :a :b | a name <= b name ].
	groups
		addAll:
			(groupSpecs
				collect: [ :grpSpec | 
					(MBGroupInfo spec: grpSpec)
						configurationInfo: self configurationInfo;
						yourself ]).
	^ groups
]

{ #category : #accessing-computed }
MBAbstractInfoList >> infoList [
	infoList ~~ nil ifTrue: [ ^infoList ].
	^ infoList := self calculateInfoList
]

{ #category : #accessing-computed }
MBAbstractInfoList >> infos [
	"Return the list of info that are displayed"
	^self infoList
]

{ #category : #updating }
MBAbstractInfoList >> interestedInPackage: operation [
	"potential currentVersion change"
	
	self fullRecalculate.
	^true
]

{ #category : #updating }
MBAbstractInfoList >> interestedInPackageNamed: packageName [
	packages notNil
		ifTrue: [ 
			(self packages detect: [ :info | info interestedInPackageNamed: packageName ] ifNone: [  ]) notNil
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #testing }
MBAbstractInfoList >> isDirty [
	^ self packages anySatisfy: [:each | each isDirty ]
]

{ #category : #accessing-computed }
MBAbstractInfoList >> packages [
	"Return the list of dependent packages for the last baseline"

	| packageSpecs |
	packages ~~ nil
		ifTrue: [ ^ packages ].
	self versionForInfoList ifNil: [ ^ #() ].
	packages := OrderedCollection new.
	packageSpecs := self versionForInfoList packages.
	packageSpecs := packageSpecs asSortedCollection: [ :a :b | a name <= b name ].
	packages
		addAll:
			(packageSpecs
				collect: [ :pkgSpec | 
					| packageName |
					(packageName := pkgSpec getFile) ifNil: [ packageName := pkgSpec name asString ].
					(MBPackageInfo named: packageName packageName: pkgSpec name asString spec: pkgSpec)
						configurationInfo: self configurationInfo;
						yourself ]).
	^ packages
]

{ #category : #accessing-computed }
MBAbstractInfoList >> projects [
	"Return the list of dependent packages for the last baseline"

	| projectSpecs |
	projects
		ifNotNil: [ ^ projects ].
	projects := OrderedCollection new.
	self versionForInfoList ifNil: [ ^ projects ].
	projectSpecs := self versionForInfoList projects.
	projectSpecs := projectSpecs asSortedCollection: [ :a :b | a name <= b name ].
	projects
		addAll:
			(projectSpecs
				collect: [ :prjSpec | 
					(MBProjectInfo named: prjSpec name packageName: prjSpec projectPackage name spec: prjSpec)
						configurationInfo: self configurationInfo;
						yourself ]).
	^ projects
]

{ #category : #updating }
MBAbstractInfoList >> recalculate [

	text := groups := validationResult := validationIcon := projects:= packages := nil.
	infoList == nil ifTrue: [ ^self ].
	self announcer announce: (MBInfoListChanged changed: self).
	infoList := nil
]

{ #category : #updating }
MBAbstractInfoList >> recalculateIfInterestedInPackageNamed: packageName for: operation [
	"keep projects separate, since I'm the only one interested in projects."
	(self interestedInPackageNamed: packageName)
		ifTrue: [ ^self interestedInPackage: operation ].
	projects notNil
		ifTrue: [ 
			(self projects detect: [ :info | info interestedInPackageNamed: packageName ] ifNone: [  ]) notNil
				ifTrue: [ ^self interestedInPackage: operation ] ].
	^false
]

{ #category : #initialize-release }
MBAbstractInfoList >> release [

	packages := projects := groups := infoList := nil.
	super release.	
]

{ #category : #accessing-computed }
MBAbstractInfoList >> versionForInfoList [

	^self subclassResponsibility 
]
