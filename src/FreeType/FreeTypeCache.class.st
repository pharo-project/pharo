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
	"answer true if the cache should be cleared on image shutdown"
	
	^true
]

{ #category : #cleanup }
FreeTypeCache class >> clearCurrent [
	"
	self clearCurrent.
	"
	current := nil
]

{ #category : #accessing }
FreeTypeCache class >> current [
	^ current ifNil: [  current := self new ]
]

{ #category : #accessing }
FreeTypeCache class >> defaultMaximumSize [
	"answer the default maximumSize in bytes"
	^1024*5000 "5 Megabytes"
]

{ #category : #'initialize-release' }
FreeTypeCache class >> initialize [
	SessionManager default 
		registerGuiClassNamed: self name
		atPriority: 10.
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
FreeTypeCache >> atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag [

	^ self
		atFont: aFreeTypeFont
		charCode: charCodeInteger
		type: typeFlag
		ifAbsentPut: [ self error: 'Not found' ]
]

{ #category : #'add-remove' }
FreeTypeCache >> atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag ifAbsentPut: aBlock [ 
	| charCodeTable typeTable entry v vSize |

	aFreeTypeFont mutex criticalReleasingOnError: [  				
		charCodeTable := fontTable at: aFreeTypeFont ifAbsentPut:[self dictionaryClass new: 60].
		typeTable := charCodeTable at: charCodeInteger ifAbsentPut:[self dictionaryClass new: 10].
		entry := typeTable at: typeFlag ifAbsent:[].
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
			object: v;
			yourself).
		typeTable at: typeFlag put: entry.
		fifo addLast: entry.
		maximumSize ifNotNil:[self shrinkTo: maximumSize].
		^v ]
	
]

{ #category : #'add-remove' }
FreeTypeCache >> atFont: aFreeTypeFont charCode: charCodeInteger type: typeFlag put: anObject [ 
	| charCodeTable typeTable anObjectSize oldEntry oldEntrySize entry |

	aFreeTypeFont mutex criticalReleasingOnError: [  					
		anObjectSize := self sizeOf: anObject.
		(maximumSize notNil and:[anObjectSize > maximumSize]) 
			ifTrue:[^anObject].  
		(charCodeTable := fontTable at: aFreeTypeFont ifAbsentPut:[self dictionaryClass new: 60])
			ifNotNil:[
				(typeTable := charCodeTable at: charCodeInteger ifAbsentPut:[self dictionaryClass new: 10])
					ifNotNil:[
						oldEntry := typeTable at: typeFlag ifAbsent:[].
						oldEntrySize := (oldEntry 
							ifNil:[0] 
							ifNotNil:[self sizeOf: oldEntry object]). 
						entry := (self fifoEntryClass new
							font: aFreeTypeFont;
							charCode: charCodeInteger;
							type: typeFlag;
							object: anObject;
							yourself).
						typeTable at: typeFlag put: entry]].
		used := used + anObjectSize - oldEntrySize.
		oldEntry ifNotNil: [fifo remove: oldEntry].
		fifo addLast: entry.
		maximumSize ifNotNil:[self shrinkTo: maximumSize].
		^anObject]
	
]

{ #category : #public }
FreeTypeCache >> cacheSize [
	^ self maximumSize / 1024
]

{ #category : #public }
FreeTypeCache >> cacheSize: anInteger [
	self maximumSize: (anInteger * 1024)
]

{ #category : #private }
FreeTypeCache >> dictionaryClass [
	^Dictionary
]

{ #category : #private }
FreeTypeCache >> fifoClass [
	^FreeTypeCacheLinkedList
]

{ #category : #private }
FreeTypeCache >> fifoEntryClass [
	^FreeTypeCacheEntry
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
	used := 0.

	
]

{ #category : #'add-remove' }
FreeTypeCache >> removeAllForFont: aFreeTypeFont [
	| toRemove |
	
	(fontTable includesKey: aFreeTypeFont) ifFalse:[^self].
	toRemove := IdentitySet new.
	fifo do:[:entry |
		entry font = aFreeTypeFont
			ifTrue:[toRemove add: entry]].
	toRemove do:[:entry | | d | 
		fifo remove: entry.
		d := (fontTable at: entry font) at: entry charCode.
		d removeKey: entry type.	
		used := used - (self sizeOf: entry object) ]. 

	
]

{ #category : #'add-remove' }
FreeTypeCache >> removeAllForType: typeFlag [
	| toRemove |
	
	toRemove := IdentitySet new.
	fifo do:[:entry |
		entry type = typeFlag
			ifTrue:[toRemove add: entry]].
	toRemove do:[:entry | | d | 
		fifo remove: entry.
		d := (fontTable at: entry font) at: entry charCode.
		d removeKey: entry type.	
		used := used - (self sizeOf: entry object) ]. 

	
]

{ #category : #public }
FreeTypeCache >> report [
	"answer a description of the current state of the cache"
	| usedPercent |
	usedPercent := maximumSize 
		ifNil: [0]
		ifNotNil: [(used * 100 / maximumSize) asFloat rounded].
	^usedPercent asString,'% Full (maximumSize: ', maximumSize asString, ' , used: ', used asString,')'
]

{ #category : #private }
FreeTypeCache >> shrinkTo: newSize [
	"if the used size is greater than newSize, then remove all the receiver's entries"
	
	used > newSize ifTrue:[self removeAll]
]

{ #category : #public }
FreeTypeCache >> sizeOf: anObject [
	^(anObject isKindOf: Form)
		ifTrue:[(anObject bitsSize * 4) + 32]
		ifFalse:[4]
	
]
