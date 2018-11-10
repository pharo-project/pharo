"
I'm a dialog to register new repositories.
I can create new repositories, add existing ones and clone for different places (including a generic url clone).

self new 
	onAccept: [ :newRepository | newRepository register ];
	openDialogWithSpec
"
Class {
	#name : #IceTipRegisterRepositoryDialog,
	#superclass : #IceTipOptionDialog,
	#instVars : [
		'types',
		'repository',
		'acceptBlock'
	],
	#category : #'Iceberg-TipUI-View-RepositoryType'
}

{ #category : #specs }
IceTipRegisterRepositoryDialog class >> title [
	^ 'Register project'
]

{ #category : #accessing }
IceTipRegisterRepositoryDialog >> allTypes [
	^ types ifNil: [ types := IceTipRepositoryTypePanel allTypes ]
]

{ #category : #accessing }
IceTipRegisterRepositoryDialog >> beForCloneOfRepository: aRepository [
	self withoutNewRepository.
	self withoutLocalRepository.
	repository := aRepository
]

{ #category : #actions }
IceTipRegisterRepositoryDialog >> doAccept [
	| newRepository |
	
	self selectedType validate.
	self isEditing ifTrue: [ self selectedType repository: repository ].
	newRepository := self selectedType newRepository.
	self isEditing ifFalse: [ newRepository register ].
	acceptBlock ifNotNil: [ 
		acceptBlock cull: self ]
]

{ #category : #actions }
IceTipRegisterRepositoryDialog >> doEdit: aRepository [
	repository 
		location: aRepository location;
		subdirectory: aRepository subdirectory
]

{ #category : #testing }
IceTipRegisterRepositoryDialog >> isEditing [ 
	^ repository isNil not
]

{ #category : #events }
IceTipRegisterRepositoryDialog >> onAccept: aBlock [
	acceptBlock := aBlock
]

{ #category : #accessing }
IceTipRegisterRepositoryDialog >> withoutLocalRepository [
	types := self allTypes reject: #isLocalRepository.
	typeList widget dataSource elements: types.
	typeList widget refresh
]

{ #category : #accessing }
IceTipRegisterRepositoryDialog >> withoutNewRepository [
	types := self allTypes reject: #isNewRepository.
	typeList widget dataSource elements: types.
	typeList widget refresh
]
