"
Cache for freetype fonts
"
Class {
	#name : #FreeTypeCache,
	#superclass : #Object,
	#instVars : [
		'maximumSize',
		'used',
		'fontTable',
		'fifo'
	],
	#pools : [
		'FreeTypeCacheConstants'
	],
	#classInstVars : [
		'current'
	],
	#category : #'FreeType-Cache'
}

{ #category : #cleanup }
FreeTypeCache class >> cleanUp [

	self clearCurrent
]

{ #category : #'system startup' }
FreeTypeCache class >> clearCacheOnShutdown [
	"Answer true if the cache should be cleared on image shutdown"

	^ true
]

{ #category : #'private - cleaning' }
FreeTypeCache class >> clearCurrent [
	<script>

	current := nil
]

{ #category : #accessing }
FreeTypeCache class >> current [

	^ current ifNil: [  current := self new ]
]

{ #category : #accessing }
FreeTypeCache class >> defaultMaximumSize [
	"Answer the default maximumSize in bytes"

	^1024*5000 "5 Megabytes"
]

{ #category : #'class initialization' }
FreeTypeCache class >> initialize [

	SessionManager default
		registerGuiClassNamed: self name
		atPriority: 10
]

{ #category : #'system startup' }
FreeTypeCache class >> shutDown: isImageQuitting [

	isImageQuitting ifFalse: [ ^ self ].
	(current notNil and: [ self clearCacheOnShutdown ])
		ifTrue:[
			self current removeAll.
			current := nil ]
]

{ #category : #'system startup' }
FreeTypeCache class >> startUp: isImageStarting [

	isImageStarting ifFalse: [ ^ self ].
	self current removeAll.
	current := nil
]

{ #category : #'add-remove' }
FreeTypeCache >> atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag scale: scale [

	^ self
		atFont: aFreeTypeFont
		charCode: charCodeInteger
		type: typeFlag
		scale: scale
		ifAbsentPut: [ self error: 'Not found' ]
]

{ #category : #'add-remove' }
FreeTypeCache >> atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag scale: scale ifAbsentPut: aBlock [

	| charCodeTable typeTable scaleTable entry v vSize |

	aFreeTypeFont mutex criticalReleasingOnError: [
		charCodeTable := fontTable at: aFreeTypeFont ifAbsentPut:[self dictionaryClass new: 60].
		typeTable := charCodeTable at: charCodeInteger ifAbsentPut:[self dictionaryClass new: 10].
		scaleTable := typeTable at: typeFlag ifAbsentPut:[self dictionaryClass new: 1].
		entry := scaleTable at: scale ifAbsent:[].
		entry
			ifNotNil:[
				fifo moveDown: entry.
				^entry object].
		v := aBlock value.
		vSize := self sizeOf: v.
		(maximumSize notNil and:[vSize > maximumSize])
			ifTrue:[^v].
		used := used + vSize.
		entry := (self fifoEntryClass new
			 font: aFreeTypeFont;
			charCode: charCodeInteger;
			type: typeFlag;
			scale: scale;
			object: v;
			yourself).
		scaleTable at: scale put: entry.
		fifo addLast: entry.
		maximumSize ifNotNil:[self shrinkTo: maximumSize].
		^ v ]
]

{ #category : #'add-remove' }
FreeTypeCache >> atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag scale: scale put: anObject [

	| charCodeTable typeTable scaleTable anObjectSize oldEntry oldEntrySize entry |

	aFreeTypeFont mutex criticalReleasingOnError: [
		anObjectSize := self sizeOf: anObject.
		(maximumSize notNil and:[anObjectSize > maximumSize])
			ifTrue:[^anObject].
		(charCodeTable := fontTable at: aFreeTypeFont ifAbsentPut:[self dictionaryClass new: 60])
			ifNotNil:[
				(typeTable := charCodeTable at: charCodeInteger ifAbsentPut:[self dictionaryClass new: 10])
					ifNotNil:[
						scaleTable := typeTable at: typeFlag ifAbsentPut:[self dictionaryClass new: 1].
						oldEntry := scaleTable at: scale ifAbsent:[].
						oldEntrySize := (oldEntry
							ifNil:[0]
							ifNotNil:[self sizeOf: oldEntry object]).
						entry := (self fifoEntryClass new
							font: aFreeTypeFont;
							charCode: charCodeInteger;
							type: typeFlag;
							scale: scale;
							object: anObject;
							yourself).
						scaleTable at: scale put: entry]].
		used := used + anObjectSize - oldEntrySize.
		oldEntry ifNotNil: [fifo remove: oldEntry].
		fifo addLast: entry.
		maximumSize ifNotNil:[self shrinkTo: maximumSize].
		^ anObject ]
]

{ #category : #public }
FreeTypeCache >> cacheSize [

	^ self maximumSize / 1024
]

{ #category : #public }
FreeTypeCache >> cacheSize: anInteger [

	self maximumSize: (anInteger * 1024)
]

{ #category : #'private - accessing' }
FreeTypeCache >> dictionaryClass [

	^ Dictionary
]

{ #category : #'private - accessing' }
FreeTypeCache >> fifoClass [

	^ FreeTypeCacheLinkedList
]

{ #category : #'private - accessing' }
FreeTypeCache >> fifoEntryClass [

	^ FreeTypeCacheEntry
]

{ #category : #initialization }
FreeTypeCache >> initialize [

	super initialize.
	maximumSize := self class defaultMaximumSize.
	fontTable := self dictionaryClass new: 100.
	used := 0.
	fifo := self fifoClass new
]

{ #category : #public }
FreeTypeCache >> maximumSize [

	^ maximumSize
]

{ #category : #public }
FreeTypeCache >> maximumSize: anIntegerOrNil [

	maximumSize := anIntegerOrNil.
	maximumSize ifNotNil:[
		used > maximumSize
			ifTrue:["shrink"
				self shrinkTo: maximumSize]]
]

{ #category : #'add-remove' }
FreeTypeCache >> removeAll [

	fontTable := self dictionaryClass new: 100.
	fifo := self fifoClass new.
	used := 0
]

{ #category : #'add-remove' }
FreeTypeCache >> removeAllForFont: aFreeTypeFont [

	| toRemove |
	(fontTable includesKey: aFreeTypeFont) ifFalse: [ ^ self ].
	toRemove := IdentitySet new.
	fifo do: [ :entry |
		entry font = aFreeTypeFont ifTrue: [ toRemove add: entry ] ].
	toRemove do: [ :entry |
		| d |
		fifo remove: entry.
		d := (fontTable at: entry font) at: entry charCode.
		d removeKey: entry type.
		used := used - (self sizeOf: entry object) ]
]

{ #category : #'add-remove' }
FreeTypeCache >> removeAllForType: typeFlag [

	| toRemove |
	toRemove := IdentitySet new.
	fifo do: [ :entry |
		entry type = typeFlag ifTrue: [ toRemove add: entry ] ].
	toRemove do: [ :entry |
		| d |
		fifo remove: entry.
		d := (fontTable at: entry font) at: entry charCode.
		d removeKey: entry type.
		used := used - (self sizeOf: entry object) ]
]

{ #category : #reporting }
FreeTypeCache >> reportCacheState [
	"Answer a description of the current state of the cache"

	| usedPercent |
	usedPercent := maximumSize
		ifNil: [0]
		ifNotNil: [(used * 100 / maximumSize) asFloat rounded].
	^ usedPercent asString,'% Full (maximumSize: ', maximumSize asString, ' , used: ', used asString,')'
]

{ #category : #'private - sizing' }
FreeTypeCache >> shrinkTo: newSize [
	"if the used size is greater than newSize, then remove all the receiver's entries"

	used > newSize ifTrue:[self removeAll]
]

{ #category : #'private - accessing' }
FreeTypeCache >> sizeOf: anObject [

	^ anObject ftSize
]
