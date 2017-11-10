"
A GroupsHolder is a simple object which manage a collection of groups
"
Class {
	#name : #GroupHolder,
	#superclass : #Object,
	#instVars : [
		'groups'
	],
	#category : #Nautilus-GroupManager
}

{ #category : #adding }
GroupHolder >> add: aCollection into: aGroup [

	aGroup
		ifAllowed: [ aGroup addAll: aCollection ]
		ifNot: [ self openReadOnlyError ]
]

{ #category : #adding }
GroupHolder >> addAGroup: aGroup [

	(self includesAGroupNamed: aGroup name)
		ifTrue: [ 
			self openError: aGroup name.
			^ nil].
	self groups add: aGroup.
	GroupAnnouncer uniqueInstance announce: ( AGroupHasBeenAdded group: aGroup into: self ).
]

{ #category : #adding }
GroupHolder >> addAMethodGroupSilentlyNamed: entry methods: aCollectionOfMethods [
	(self includesAGroupNamed: entry)
		ifTrue: [ ^ nil ]
		ifFalse: [ 
			| group |
			group := self groups add: (MethodGroup named: entry methods: aCollectionOfMethods).
			^ group ]
]

{ #category : #adding }
GroupHolder >> addAPackageGroupSilentlyNamed: entry packages: aCollectionOfPackages [
	(self includesAGroupNamed: entry)
		ifTrue: [ ^ self openError: entry name ]
		ifFalse: [ 
			| group |
			group := self groups add: (PackageGroup named: entry packages: aCollectionOfPackages).
			^ group ]
]

{ #category : #adding }
GroupHolder >> addAnEmptyDynamicGroupNamed: aName [

	^ (self addAPackageGroupSilentlyNamed: aName packages: OrderedCollection new)
			isFillable: true;
			yourself
]

{ #category : #adding }
GroupHolder >> createAnEmptyStaticGroup [

	| entry |
	entry := UIManager default request: 'Name of the new group:' initialAnswer: '' title: 'Create a new group'.
	^ (entry isNil or: [entry isEmpty])
		ifFalse: [ self addAnEmptyDynamicGroupNamed: entry ]
]

{ #category : #protocol }
GroupHolder >> groupNamed: aString [
	^ self groups detect: [:each | each name = aString]
]

{ #category : #protocol }
GroupHolder >> groups [

	^  groups ifNil: [ groups := OrderedCollection new ]
]

{ #category : #protocol }
GroupHolder >> groups: aCollectionOfGroups [
	groups  := aCollectionOfGroups 
]

{ #category : #protocol }
GroupHolder >> groupsNames [

	^ self groups collect: [:group | group name ]
]

{ #category : #protocol }
GroupHolder >> includes: aGroup [

	^ self groups includes: aGroup
]

{ #category : #protocol }
GroupHolder >> includesAGroupNamed: aName [

	^ self groupsNames includes: aName
]

{ #category : #protocol }
GroupHolder >> isEmpty [

	^ self groups isEmpty.
]

{ #category : #windows }
GroupHolder >> openError: name [

	(GroupAlreadyExists groupName: name) signal
]

{ #category : #windows }
GroupHolder >> openReadOnlyError [

	UIManager default  alert: 'This group is read only' title: 'Access error'
]

{ #category : #windows }
GroupHolder >> openRemoveDialogOn: aGroup [
	^ UIManager default confirm: 'Do you really want to delete the group named ' , aGroup name , ' ?'
]

{ #category : #protocol }
GroupHolder >> remove: aPackage from: aGroup [

	^ aGroup
		ifAllowed: [ aGroup removePackages: { aPackage } ]
		ifNot: [ self openReadOnlyError ]
]

{ #category : #removing }
GroupHolder >> removeAGroup: aGroup [

	| yesIWantToRemoveTheGroup |
	yesIWantToRemoveTheGroup := self openRemoveDialogOn: aGroup.
	yesIWantToRemoveTheGroup
		ifFalse: [ ^ nil ].
	^ self removeAGroupSilently: aGroup

]

{ #category : #removing }
GroupHolder >> removeAGroupSilently: aGroup [

	| group |
	aGroup ifNil: [ ^ self ].
	 aGroup removable
		ifFalse: [ ^ self openReadOnlyError ].
	group :=  self groups remove: aGroup ifAbsent: [].
	GroupAnnouncer uniqueInstance announce: ( AGroupHasBeenRemoved group: aGroup from: self ).
	^ group
]

{ #category : #removing }
GroupHolder >> renameAGroup: aGroup [

	| entry |
	entry := UIManager default request: 'New name of the group' initialAnswer: aGroup name title: 'Rename a group'.
	(entry isNil or: [entry isEmpty]) ifTrue: [^ aGroup].
	(self includesAGroupNamed: entry)
		ifTrue: [ self inform: 'Renaming aborded : ', entry, ' is already taken' ].
	aGroup isReadOnly
		ifTrue: [ ^ self openReadOnlyError ].
	aGroup name: entry.
	GroupAnnouncer uniqueInstance announce: ( AGroupHasBeenRenamed group: aGroup from: self ).
	^ aGroup	

]

{ #category : #protocol }
GroupHolder >> sort: aBlock [

	^ groups := self groups sort: aBlock
]
