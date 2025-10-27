"
I represent the creation of a class.
"
Class {
	#name : 'EpClassAddition',
	#superclass : 'EpClassChange',
	#instVars : [
		'class'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpClassAddition class >> class: aClass [

	^ self basicNew
		initializeClass: aClass;
		yourself
]

{ #category : 'visitor' }
EpClassAddition >> accept: aVisitor [

	^ aVisitor visitClassAddition: self
]

{ #category : 'accessing' }
EpClassAddition >> behaviorAffected [

	^ class
]

{ #category : 'accessing' }
EpClassAddition >> behaviorAffectedName [

	^ class name
]

{ #category : 'accessing' }
EpClassAddition >> classAdded [

	^ class
]

{ #category : 'accessing' }
EpClassAddition >> fileOutEntryOn: aWriteStream [
	"File out to aWriteStream if the affected class was not deleted already"

	self behaviorAffected realClass ifNotNil: [ :realClass | 
		| exporter |		
		exporter := CodeExporter onStream: aWriteStream.
		exporter writeClassDefinition: realClass.
		exporter writeClassCommentOf: realClass.
		aWriteStream cr ]
]

{ #category : 'initialization' }
EpClassAddition >> initializeClass: aClass [
	self initialize.
	class := aClass asEpiceaRingDefinition
]
