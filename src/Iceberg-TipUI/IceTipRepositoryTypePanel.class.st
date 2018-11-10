"
I'm a generic repository type panel. 
I'm used as part of the creation dialog and to edit repository properties.
"
Class {
	#name : #IceTipRepositoryTypePanel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'projectLocation',
		'remoteLabel',
		'remoteInputText',
		'spacePanel',
		'repository'
	],
	#category : #'Iceberg-TipUI-View-RepositoryType'
}

{ #category : #accessing }
IceTipRepositoryTypePanel class >> allTypes [
	^ ((self allSubclasses 
		reject: #isAbstract)
		sorted: [ :a :b | a order < b order ])
		collect: #new
]

{ #category : #accessing }
IceTipRepositoryTypePanel class >> icon [
	self subclassResponsibility
]

{ #category : #testing }
IceTipRepositoryTypePanel class >> isAbstract [
	^ self = IceTipRepositoryTypePanel
]

{ #category : #accessing }
IceTipRepositoryTypePanel class >> order [
	^ 0
]

{ #category : #private }
IceTipRepositoryTypePanel >> defaultLocation [
	^ IceLibgitRepository repositoriesLocation pathString
]

{ #category : #initialization }
IceTipRepositoryTypePanel >> defaultSubdirectory [
	^ IceLibgitRepository defaultCodeSubdirectory
]

{ #category : #accessing }
IceTipRepositoryTypePanel >> icon [
	^ self class icon
]

{ #category : #initialization }
IceTipRepositoryTypePanel >> initializeProjectLocation [
	projectLocation := self instantiate: IceTipDirectoryPresenter.
	projectLocation 
		label: 'Local directory';
		chooseTitle: 'Choose local repository';
		location: self defaultLocation	
]

{ #category : #initialization }
IceTipRepositoryTypePanel >> initializeRemoteURL [
	remoteLabel := self newLabel label: 'Remote URL'.
	remoteInputText := self newTextInput 
		ghostText: 'e.g., git@github.com:user/MyProject.git';	
		autoAccept: true
]

{ #category : #initialization }
IceTipRepositoryTypePanel >> initializeSpacePanel [
	"just to gain space"
	spacePanel := self newIcePanel. 
]

{ #category : #initialization }
IceTipRepositoryTypePanel >> initializeWidgets [
	self initializeProjectLocation.
	self initializeSpacePanel
]

{ #category : #testing }
IceTipRepositoryTypePanel >> isLocalRepository [
	^ false
]

{ #category : #testing }
IceTipRepositoryTypePanel >> isNewRepository [
	^ false
]

{ #category : #accessing }
IceTipRepositoryTypePanel >> newRepository [
	^ self subclassResponsibility
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> projectLocation [
	^ projectLocation
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> projectLocation: anObject [
	projectLocation := anObject
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> remoteInputText [
	^ remoteInputText
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> remoteInputText: anObject [
	remoteInputText := anObject
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> remoteLabel [
	^ remoteLabel
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> remoteLabel: anObject [
	remoteLabel := anObject
]

{ #category : #accessing }
IceTipRepositoryTypePanel >> repository: aRepository [
	"Used if editing an existing repository"
	repository := aRepository
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> spacePanel [
	^ spacePanel
]

{ #category : #'accessing ui' }
IceTipRepositoryTypePanel >> spacePanel: anObject [
	spacePanel := anObject
]

{ #category : #accessing }
IceTipRepositoryTypePanel >> validate [
	^ self subclassResponsibility
]
