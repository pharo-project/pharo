"
I represent a basic event for the event recorder.
"
Class {
	#name : #GTBasicRecorderEvent,
	#superclass : #Object,
	#instVars : [
		'info',
		'timestamp'
	],
	#category : #'GT-EventRecorder-Core'
}

{ #category : #comparing }
GTBasicRecorderEvent >> = otherEvent [

	^ self class == otherEvent class
	and: [ (self info = otherEvent info)
	and: [ self timestamp = otherEvent timestamp ] ].
]

{ #category : #'accessing - announcement info' }
GTBasicRecorderEvent >> announcementTimestamp [
	^ self infoAt: #timestamp ifAbsent: [ self timestamp ]
]

{ #category : #'accessing - announcement info' }
GTBasicRecorderEvent >> computerUUID [
	^ self infoAt: #computerUUID ifAbsent: [ GlobalIdentifier uniqueInstance computerUUID asString ]
]

{ #category : #'accessing - announcement info' }
GTBasicRecorderEvent >> eventRecorderVersion [
	^ self infoAt: #eventRecorderVersion ifAbsent: [ GTEventPacking new version ]
]

{ #category : #comparing }
GTBasicRecorderEvent >> hash [
	^ self info hash bitXor: self timestamp hash
]

{ #category : #accessing }
GTBasicRecorderEvent >> hashForText: aString [
	^ self settings obfuscateIfNecessary: aString
]

{ #category : #'accessing - announcement info' }
GTBasicRecorderEvent >> imageHash [
	^ self infoAt: #imageHash ifAbsent: [ SmalltalkImage current imagePath hash ]
]

{ #category : #'accessing - announcement info' }
GTBasicRecorderEvent >> imageVersion [
	^ self infoAt: #imageVersion ifAbsent: [ SystemVersion current version ]
]

{ #category : #accessing }
GTBasicRecorderEvent >> info [
	^ info ifNil: [ info := Dictionary new ].
]

{ #category : #accessing }
GTBasicRecorderEvent >> infoAt: aSymbol [
	^ info at: aSymbol
]

{ #category : #accessing }
GTBasicRecorderEvent >> infoAt: key ifAbsent: absentBlock [
	info ifNil: [ ^ absentBlock value].
	^ info at: key ifAbsent: absentBlock
]

{ #category : #accessing }
GTBasicRecorderEvent >> infoAt: key ifAbsentPut: putBlock [
	info ifNil: [ info := Dictionary new ].
	^ info at: key ifAbsentPut: putBlock
]

{ #category : #accessing }
GTBasicRecorderEvent >> infoAt: key ifPresent: presentBlock ifAbsent: absentBlock [
	info ifNil: [ ^ absentBlock value].
	^ info at: key ifPresent: presentBlock ifAbsent: absentBlock
]

{ #category : #accessing }
GTBasicRecorderEvent >> infoAt: key put: value [
	info ifNil: [ info := Dictionary new ].
	^ info at: key put: value
]

{ #category : #accessing }
GTBasicRecorderEvent >> infoAt: key putHashForText: valueString [ 
	^ self 
		infoAt: key 
		put: (self hashForText: valueString)
]

{ #category : #initialization }
GTBasicRecorderEvent >> initialize [
	super initialize.
	timestamp := DateAndTime now.
]

{ #category : #initialization }
GTBasicRecorderEvent >> initializeObfuscated [
	self obfuscated: Privacy sendEntityNames not.
]

{ #category : #'accessing - announcement info' }
GTBasicRecorderEvent >> latestUpdate [
	^ self infoAt: #latestUpdate ifAbsent: [ SystemVersion current highestUpdate ]
]

{ #category : #accessing }
GTBasicRecorderEvent >> obfuscated [
	^ self infoAt: #obfuscated ifAbsent: true
]

{ #category : #accessing }
GTBasicRecorderEvent >> obfuscated: aBoolean [
	^ self infoAt: #obfuscated put: aBoolean
]

{ #category : #'accessing - announcement info' }
GTBasicRecorderEvent >> sessionCreationTime [
	^ self infoAt: #sessionCreationTime ifAbsent: [ SmalltalkImage current session creationTime ]
]

{ #category : #accessing }
GTBasicRecorderEvent >> settings [
	^ GlobalIdentifier uniqueInstance
]

{ #category : #accessing }
GTBasicRecorderEvent >> timestamp [
	^ timestamp
]
