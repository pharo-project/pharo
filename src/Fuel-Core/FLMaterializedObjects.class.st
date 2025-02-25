"
I am a wrapper for the results of a serialization.

I make it easy to access all objects that might be of interest after a materialization:
- the header
- the additional objects stored in the header
- the root object
When the materialization materialized multiple root objects (see FLMaterializer>>materializeObjectCollection) I hold collections of headers and root objects, with #header and #root answering the respective objects from the last materialization.

Instances of myself is answered by FLMaterializer>>materialize, #materializeObjectCollection and #materializeHeader.

Use #header, #root and #additionalObjectAt: to retrieve objects from a materialization. If my instance was the answer to #materializeObjectCollection you can retrieve all of the objects with #objects and the respective headers with #headers.
"
Class {
	#name : 'FLMaterializedObjects',
	#superclass : 'Object',
	#instVars : [
		'objects',
		'roots',
		'headers'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
FLMaterializedObjects >> addHeader: aHeader [
	headers add: aHeader
]

{ #category : 'accessing' }
FLMaterializedObjects >> addObjects: aCollection [
	objects add: aCollection
]

{ #category : 'accessing' }
FLMaterializedObjects >> addRoot: anObject [
	roots add: anObject
]

{ #category : 'accessing' }
FLMaterializedObjects >> additionalObjectAt: anObject [
	^ self header additionalObjectAt: anObject
]

{ #category : 'accessing' }
FLMaterializedObjects >> header [
	^ headers last
]

{ #category : 'initialization' }
FLMaterializedObjects >> initialize [
	super initialize.
	
	roots := OrderedCollection new.
	headers := OrderedCollection new.
	objects := OrderedCollection new
]

{ #category : 'accessing' }
FLMaterializedObjects >> objects [
	^ objects last
]

{ #category : 'accessing' }
FLMaterializedObjects >> root [
	^ roots last
]

{ #category : 'accessing' }
FLMaterializedObjects >> roots [
	^ roots
]
