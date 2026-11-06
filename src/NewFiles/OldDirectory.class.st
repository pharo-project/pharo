"
I provide legacy file plugin support.
"
Class {
	#name : 'OldDirectory',
	#superclass : 'FFIExternalObject',
	#instVars : [
		'dirPointer',
		'entryData'
	],
	#pools : [
		'NewFileConstants'
	],
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'creation and destruction' }
OldDirectory class >> contentsOf: path [
	| directory |
	directory := self open: path.
	directory ifNil: [
		self error: 'Failed to open directory ' , path
	].

	^ [ 
		directory contents.
	] ensure: [ 
		directory close
	]
]

{ #category : 'creation and destruction' }
OldDirectory class >> create: path [
	^ File createDirectory: path
]

{ #category : 'creation and destruction' }
OldDirectory class >> open: path [
	| dirHandles dirPointer entryData |
	dirHandles := File primOpendir: '.'.
	dirHandles ifNil:  [ ^ nil ].
	
	self assert: (dirHandles isArray and: [ dirHandles size = 3 ]).
	dirPointer := dirHandles at: 3.
	entryData := dirHandles.

	^ self new
		initializeWithDirPointer: dirPointer andEntryData: entryData;
		yourself
]

{ #category : 'creation and destruction' }
OldDirectory class >> removeEmpty: path [
	^ File deleteDirectory: path
]

{ #category : 'accessing' }
OldDirectory >> close [
	File primClosedir: dirPointer
]

{ #category : 'accessing' }
OldDirectory >> contents [
	^ Array streamContents: [:out |
		| next	 |
		[ (next := self next) ~~ nil ] whileTrue: [
			out nextPut: next
		]	
	]
]

{ #category : 'initialization' }
OldDirectory >> initializeWithDirPointer: aDirPointer andEntryData: anEntryData [
	super initialize.
	dirPointer := aDirPointer.
	entryData := anEntryData..
]

{ #category : 'accessing' }
OldDirectory >> next [
	| entryResult |
	entryResult := entryData.
	entryResult	ifNotNil: [ 
		entryData := nil.
		^ File decodePathString: entryResult first
	].

	entryResult := File primReaddir: dirPointer.
	entryResult ifNil: [ ^ nil ].
	
	^ File decodePathString: entryResult first
]
