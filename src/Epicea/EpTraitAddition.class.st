"
I represent the creation of a trait.
"
Class {
	#name : 'EpTraitAddition',
	#superclass : 'EpTraitChange',
	#instVars : [
		'trait'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpTraitAddition class >> trait: aTrait [

	^ self basicNew
		initializeWithTrait: aTrait;
		yourself
]

{ #category : 'visitor' }
EpTraitAddition >> accept: aVisitor [

	^ aVisitor visitTraitAddition: self
]

{ #category : 'accessing' }
EpTraitAddition >> behaviorAffected [
	^ self traitAdded
]

{ #category : 'accessing' }
EpTraitAddition >> behaviorAffectedName [

	^ self traitAdded name
]

{ #category : 'accessing' }
EpTraitAddition >> fileOutEntryOn: aWriteStream [
	"Write a code chunk to create the receiver's trait on aWriteStream"

	| realClass exporter |
	realClass := self traitAdded realClass.
	exporter := CodeExporter onStream: aWriteStream.
	exporter writeClassDefinition: realClass.
	exporter writeClassCommentOf: realClass.
	aWriteStream cr
]

{ #category : 'initialization' }
EpTraitAddition >> initializeWithTrait: aTrait [

	self initialize.
	trait := aTrait asEpiceaRingDefinition
]

{ #category : 'accessing' }
EpTraitAddition >> traitAdded [

	^ trait
]
