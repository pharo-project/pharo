"
Common superclass for RPackage related tests
"
Class {
	#name : #RPackageTestCase,
	#superclass : #TestCase,
	#instVars : [
		'announcerForTest',
		'oldSystemAnnouncer',
		'createdClasses',
		'createdPackages',
		'createdCategories'
	],
	#category : #'RPackage-Tests'
}

{ #category : #utils }
RPackageTestCase >> allManagers [

	^ MCWorkingCopy allManagers
]

{ #category : #utility }
RPackageTestCase >> createNewClassNamed: aName [
	^ self createNewClassNamed: aName inCategory: 'RPackageTest'
]

{ #category : #utility }
RPackageTestCase >> createNewClassNamed: aName inCategory: cat [
	
	| cls |
	cls := Object subclass: aName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: cat.
	createdClasses add: cls.
	createdCategories add: cat.
	^ cls
]

{ #category : #utility }
RPackageTestCase >> createNewClassNamed: aName inPackage: p [
	
	| cls |
	cls := self createNewClassNamed: aName.
	p addClassDefinition: cls.
	^ cls
]

{ #category : #utility }
RPackageTestCase >> createNewPackageNamed: aName [

	| pack |
	self removePackage: aName.
	pack := self packageClass named: aName.
	createdPackages add: pack.
	^ pack
]

{ #category : #utility }
RPackageTestCase >> createNewTraitNamed: aName [
	^ self createNewTraitNamed: aName inCategory: 'RPackageTest'
]

{ #category : #utility }
RPackageTestCase >> createNewTraitNamed: aName inCategory: cat [
	
	| cls |
	cls := Trait named: aName
			uses: {}
			category: cat.
	createdClasses add: cls.
	createdCategories add: cat.
	^ cls
]

{ #category : #utility }
RPackageTestCase >> createNewTraitNamed: aName inPackage: p [
	
	| cls |
	cls := self createNewTraitNamed: aName.
	p addClassDefinition: cls.
	^ cls
]

{ #category : #'announcer handling' }
RPackageTestCase >> initializeAnnouncers [
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.

	SystemAnnouncer announcer: (announcerForTest := SystemAnnouncer new).
]

{ #category : #accessing }
RPackageTestCase >> organizer [
	"This method will access the organizer that will temporarily swapped using the withOrganizer:do: method"
	^ self packageClass organizer
]

{ #category : #accessing }
RPackageTestCase >> packageClass [ 
	"returns the system class that represents Packages"
	^ RPackage 
]

{ #category : #accessing }
RPackageTestCase >> packageOrganizerClass [ 
	"returns the package organizer class"
	^ RPackageOrganizer
]

{ #category : #utility }
RPackageTestCase >> removePackage: aName [
	self packageClass organizer basicUnregisterPackageNamed: aName.
	
]

{ #category : #'announcer handling' }
RPackageTestCase >> restoreAnnouncers [
	SystemAnnouncer announcer: oldSystemAnnouncer.

]

{ #category : #running }
RPackageTestCase >> runCase [
	[
	self initializeAnnouncers.
	
	^ self packageClass 
		withOrganizer: self setupOrganizer
		do: [ 
			self resources do: [:each | each availableFor: self].
			self setUp.
			self performTest
			]
	] ensure: [ 
		self tearDown.
		self restoreAnnouncers.
		self cleanUpInstanceVariables
	]
]

{ #category : #running }
RPackageTestCase >> setUp [
	super setUp.
	createdClasses := OrderedCollection new.
	createdPackages := OrderedCollection new.
	createdCategories := Set new.
]

{ #category : #accessing }
RPackageTestCase >> setupOrganizer [
	^ self packageOrganizerClass basicNew initialize; debuggingName: 'Organizer for RPackageTest'; yourself
]
