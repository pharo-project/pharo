"
I'm a repair option.
Repositories can be broken in several ways. My children will describe them and take the actions needed to perform the re-alignment of an image with its repository.
"
Class {
	#name : #IceTipRepairAction,
	#superclass : #IceTipAction,
	#instVars : [
		'repositoryModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairAction class >> allAvailableFor: aRepository [
	^ ((self allSubclasses 
		select: [ :each | each isAvailableFor: aRepository ])
		sorted: [ :a :b | a order < b order ])
		collect: #new
]

{ #category : #accessing }
IceTipRepairAction class >> help [
	^ self subclassResponsibility
]

{ #category : #accessing }
IceTipRepairAction class >> icon [
	^ self subclassResponsibility
]

{ #category : #testing }
IceTipRepairAction class >> isAvailableFor: aRepository [
	^ false
]

{ #category : #accessing }
IceTipRepairAction class >> order [
	^ self subclassResponsibility
]

{ #category : #accessing }
IceTipRepairAction class >> title [
	^ self subclassResponsibility
]

{ #category : #executing }
IceTipRepairAction >> executeWith: aRepositoryModel [
	self repositoryModel: aRepositoryModel.
	self execute
]

{ #category : #private }
IceTipRepairAction >> fetchAllRemotes [
	| remotes |
	remotes := self repository remotes.
	UIManager default informUserDuring: [ :bar | 
		bar label: 'Fetching incoming commits'.
		bar max: remotes size.
		bar value: 1.
		remotes withIndexDo: [ :each :index |
			bar label: ('Fetching incoming commits of {1}' format: {each name}).
			bar value: index + 1.
			self repository fetchFrom: each ] ].	
]

{ #category : #accessing }
IceTipRepairAction >> help [ 
	^ self class help
]

{ #category : #accessing }
IceTipRepairAction >> icon [
	^ self class icon
]

{ #category : #accessing }
IceTipRepairAction >> repository [
	^ self repositoryModel entity
]

{ #category : #accessing }
IceTipRepairAction >> repositoryModel [
	^ repositoryModel
]

{ #category : #accessing }
IceTipRepairAction >> repositoryModel: anObject [
	repositoryModel := anObject
]

{ #category : #accessing }
IceTipRepairAction >> title [
	^ self class title
]
