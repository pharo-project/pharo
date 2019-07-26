"
I model the meta level when activating a MetaLink.
"
Class {
	#name : #RFMetaContext,
	#superclass : #Object,
	#instVars : [
		'level'
	],
	#category : #'Reflectivity-Contexts'
}

{ #category : #accessing }
RFMetaContext class >> current [ 
	ActiveMetaContext value ifNil:[ActiveMetaContext value: self basicNew initialize].
	^ActiveMetaContext value
]

{ #category : #initialization }
RFMetaContext class >> reset [
	ActiveMetaContext value: self new.
]

{ #category : #initialization }
RFMetaContext >> initialize [
 level := 0
]

{ #category : #testing }
RFMetaContext >> isActive: n [
	^level = n
]

{ #category : #testing }
RFMetaContext >> isMeta [
	^level > 0
]

{ #category : #level }
RFMetaContext >> shiftLevelDown [
	level := (level - 1) max: 0.
	self assert: level >=0
]

{ #category : #level }
RFMetaContext >> shiftLevelUp [
	level := level + 1
]
