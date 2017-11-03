"
A group containing packages
"
Class {
	#name : #PackageGroup,
	#superclass : #Group,
	#instVars : [
		'packages',
		'browsedEnvironment'
	],
	#category : #Nautilus-GroupManager
}

{ #category : #'instance creation' }
PackageGroup class >> named: aString packages: aCollectionOfPackages [
	^ self new
		name: aString;
		packages: aCollectionOfPackages;
		yourself
]

{ #category : #adding/removing }
PackageGroup >> addPackages: aCollection [
	aCollection do: [ :each | packages addIfNotPresent: each ].
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)
]

{ #category : #accessing }
PackageGroup >> browsedEnvironment [
	^ browsedEnvironment ifNil: [ browsedEnvironment := RBBrowserEnvironment new ]
]

{ #category : #queries }
PackageGroup >> classes [
	| classes |
	classes := OrderedCollection new.
	packages do: [ :package | package classes do: [ :class | classes addIfNotPresent: class ] ].
	^ classes
]

{ #category : #accessing }
PackageGroup >> elements [
	^ packages sorted
]

{ #category : #testing }
PackageGroup >> hasClassNamed: aClassName [
	^ packages anySatisfy: [ :package | package classes anySatisfy: [ :class | class name = aClassName ] ]
]

{ #category : #initialization }
PackageGroup >> initialize [
	super initialize.
	packages := OrderedCollection new. 
	self browsedEnvironment packageOrganizer announcer weak when: RPackageUnregistered send: #packageRemoved: to: self
]

{ #category : #announcements }
PackageGroup >> packageRemoved: anAnnouncement [
	packages remove: anAnnouncement package ifAbsent: [  ].
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)
]

{ #category : #accessing }
PackageGroup >> packages: aCollection [
	packages := aCollection
]

{ #category : #adding/removing }
PackageGroup >> removeAllPackages [
	packages removeAll.
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)
]

{ #category : #adding/removing }
PackageGroup >> removePackages: aCollection [
	packages removeAll: aCollection.
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)
]

{ #category : #operations }
PackageGroup >> renameTo: aNewName [
	(aNewName isNil or: [aNewName isEmpty]) ifTrue: [^ self].
	self name: aNewName
]
