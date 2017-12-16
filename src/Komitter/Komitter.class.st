"
I'm the entry point of this project.
I'm a kind of application model for the Komitter - this is why (it could be discussed) I create the UI of the Kommiter

A classic invocation is

| s k |
s := KomitStagingArea current.
k := Komitter new.
k stagingArea: s.
k open.

k lastCommit  
"
Class {
	#name : #Komitter,
	#superclass : #Model,
	#instVars : [
		'stagingArea',
		'lastCommit',
		'view'
	],
	#classInstVars : [
		'lastMessage'
	],
	#category : #'Komitter-Models'
}

{ #category : #'world menu' }
Komitter class >> komitterMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Komitter)
		action: [ self openAndCommitToMonticello ];
		order: 0.29;
		parent: #Tools;
		help: 'Cherry pick what you commit.';
		keyText: 'o, k';
		icon: self taskbarIcon.
	aBuilder withSeparatorAfter
]

{ #category : #protocol }
Komitter class >> lastMessage [

	^ lastMessage ifNil: [ MCSaveVersionDialog previousMessages first ]
]

{ #category : #protocol }
Komitter class >> lastMessage: aString [

	lastMessage := aString
]

{ #category : #protocol }
Komitter class >> openAndCommitToMonticello [

	KomitStagingArea resetCurrent.
	^ self new
		stagingArea: KomitStagingArea current;
		openAndCommitToMonticello;
		yourself
]

{ #category : #protocol }
Komitter class >> openAndCommitToMonticelloWorkingCopiesFilteredBy: aFilterBlock [
	| stagingArea |

	KomitStagingArea resetCurrent.
	stagingArea := KomitStagingArea currentFilteredBy: aFilterBlock.
	^ self new
		stagingArea: stagingArea;
		openAndCommitToMonticello;
		yourself
]

{ #category : #protocol }
Komitter class >> resetLastMessage [

	lastMessage := nil
]

{ #category : #icon }
Komitter class >> taskbarIconName [
	^#komitterIcon
]

{ #category : #initialize }
Komitter >> initialize [

	super initialize.
	self registerToAnnouncements.
	stagingArea := nil asValueHolder.
	lastCommit := nil asValueHolder.
]

{ #category : #private }
Komitter >> lastCommit [

	^ lastCommit value
]

{ #category : #private }
Komitter >> lastCommit: aKomit [

	lastCommit value: aKomit
]

{ #category : #protocol }
Komitter >> lastMessage [

	^ self class lastMessage
]

{ #category : #announcement }
Komitter >> mcPackageModified: anAnnouncement [
	| package |

	package := anAnnouncement package koPackage.
	
	package patch isEmpty
		ifTrue: [ self stagingArea removePackage: package ]
		ifFalse: [ self stagingArea addPackage: package ].
	
	view ifNil: [ ^ self ].
	
	package patch isEmpty
		ifTrue: [ view removeRoot: package ]
		ifFalse: [ view addRoot: package ]
]

{ #category : #announcement }
Komitter >> methodModified: anAnnouncement [	

	self rebuildStagingArea.
]

{ #category : #announcement }
Komitter >> methodMoved: anAnnouncement [
]

{ #category : #private }
Komitter >> newSlice [

	^ KomitNewSlice new
]

{ #category : #opening }
Komitter >> openAndCommitToMonticello [

	self openAndDo: [ self lastCommit ifNotNil: [ self lastCommit commitToMonticello ]]
]

{ #category : #opening }
Komitter >> openAndDo: aBlock [
	
	self stagingArea packages
		ifEmpty: [ ^ self inform: 'No changes to commit' ].

	view ifNil: [ view := KomitterUI new
		model: self;
		yourself ].
	
	view openDialogWithSpec
		okAction: [ | valid |
			valid := view validate.
			valid ifTrue: [ self lastCommit: view komit ].
			aBlock value.
			valid ];
		cancelAction: [ 
			self lastCommit: nil.
			view cancel ];
		centered
]

{ #category : #rebuilding }
Komitter >> rebuildStagingArea [

	self stagingArea flush.
	view model: self
]

{ #category : #announcement }
Komitter >> registerToAnnouncements [

	SystemAnnouncer uniqueInstance weak	
		when: MCPackageModified send: #rebuildStagingArea to: self;
		when: ClassAdded, ClassModifiedClassDefinition, ClassRenamed, ClassCommented send: #rebuildStagingArea to: self;
		when: ClassRepackaged send: #rebuildStagingArea to: self;
		when: ClassRemoved send: #rebuildStagingArea to: self;
		when: MethodAdded, MethodModified, MethodRecategorized send: #rebuildStagingArea to: self;
		when: MethodRepackaged send: #rebuildStagingArea to: self;
		when: MethodRemoved send: #rebuildStagingArea to: self
]

{ #category : #protocol }
Komitter >> remotes [

	^ self stagingArea remotes asOrderedCollection
		add: self newSlice;
		yourself
]

{ #category : #protocol }
Komitter >> remotesFor: aCollection [

	^ (self stagingArea remotesFor: aCollection) asOrderedCollection
		add: self newSlice;
		yourself
]

{ #category : #accessing }
Komitter >> stagingArea [

	^ stagingArea value 
]

{ #category : #accessing }
Komitter >> stagingArea: anObject [

	stagingArea value: anObject
]

{ #category : #announcement }
Komitter >> unsubscribe [
	SystemAnnouncer uniqueInstance unsubscribe: self
]
