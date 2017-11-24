"
I am a log browser item that represents an entry.
"
Class {
	#name : #EpEntryItem,
	#superclass : #Model,
	#instVars : [
		'browser',
		'entry',
		'triggerItems'
	],
	#category : #'EpiceaBrowsers-UI-Log'
}

{ #category : #comparing }
EpEntryItem >> = other [
	^ self species = other species and: [ self entry = other entry ]
]

{ #category : #converting }
EpEntryItem >> asMorph [

	| morphs |
	morphs := OrderedCollection new.
	morphs addAll: self triggerMarkMorphs.
	morphs add: self icon asMorph.
	morphs addAll: self eventMorphs.
	morphs add: self morphFactory smallGap.
	morphs addAll: self timeMorphs.
	morphs addAll: self commentMorphs.

	^ self morphFactory rowWithAll: morphs
]

{ #category : #operations }
EpEntryItem >> browseItem [ 
	self eventAccept: EpBrowseVisitor new
]

{ #category : #accessing }
EpEntryItem >> browser [
	^ browser
]

{ #category : #accessing }
EpEntryItem >> browser: anObject [
	browser := anObject
]

{ #category : #private }
EpEntryItem >> commentMorphs [
	
	browser commentsLog
		commentAt: entry 
		ifPresent: [ :comment |
			comment trim ifNotEmpty: [:trimmedComment |
				^ { self morphFactory pillWith: trimmedComment } ] ].
	^ #()
]

{ #category : #accessing }
EpEntryItem >> entry [
	^ entry
]

{ #category : #accessing }
EpEntryItem >> entry: anObject [
	entry := anObject
]

{ #category : #accessing }
EpEntryItem >> event [

	^ entry content
]

{ #category : #private }
EpEntryItem >> eventAccept: aVisitor [

	^ entry content accept: aVisitor
]

{ #category : #private }
EpEntryItem >> eventMorphs [
	
	^ self eventAccept: EpMorphVisitor new
]

{ #category : #comparing }
EpEntryItem >> hash [
	^ self species hash bitXor: self entry hash
]

{ #category : #private }
EpEntryItem >> icon [

	^ self eventAccept: EpIconVisitor new
]

{ #category : #accessing }
EpEntryItem >> log [
	^ browser log
]

{ #category : #private }
EpEntryItem >> morphFactory [

	^ EpMorphFactory default
]

{ #category : #private }
EpEntryItem >> newStateString [

	^ self eventAccept: EpNewStateVisitor new
]

{ #category : #operations }
EpEntryItem >> oldComment [

	^ browser commentsLog commentAt: entry ifAbsent: [ '' ]
]

{ #category : #private }
EpEntryItem >> oldStateString [

	^ entry content accept: EpOldStateVisitor new
]

{ #category : #printing }
EpEntryItem >> printOn: aStream [
	super printOn: aStream.

	aStream nextPut: $(.
	self entry printOn: aStream.
	aStream nextPut: $).
]

{ #category : #accessing }
EpEntryItem >> reference [
	^ self log referenceTo: entry
]

{ #category : #private }
EpEntryItem >> timeMorphs [
	
	^ self timeString
		ifEmpty: [ #() ]
		ifNotEmpty: [:aString | { self morphFactory secondaryStringFor: aString } ]
]

{ #category : #operations }
EpEntryItem >> timeString [
	| aDateTime |
	aDateTime := self log timeAt: entry ifAbsent: [ ^ '' ].
	
	^ aDateTime epiceaBrowsersAsString
]

{ #category : #accessing }
EpEntryItem >> triggerItems [
	^ triggerItems
]

{ #category : #accessing }
EpEntryItem >> triggerItems: anObject [
	triggerItems := anObject
]

{ #category : #private }
EpEntryItem >> triggerMarkMorphs [

	^ self triggerItems collect: [:item |
		self morphFactory semiTransparent: item icon ]
]
