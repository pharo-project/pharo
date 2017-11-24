"
I represent a collection of GTEventCollector objects. GTEventRecorder uses me and I am responsible for adding and removing the collectors.

Public API and Key Messages

- add:  
- remove:
 
Internal Representation and Key Implementation Points.

    Instance Variables
	collectors:		<OrderedCollection>
	mutex:		<Mutex>

"
Class {
	#name : #GTEventCollectors,
	#superclass : #Object,
	#instVars : [
		'mutex',
		'collectors'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #adding }
GTEventCollectors >> add: aGTEventCollector [
	^ mutex critical: [ 
		(collectors identityIncludes: aGTEventCollector)
			ifFalse: [ collectors add: aGTEventCollector ]
			ifTrue: [ aGTEventCollector ] ]
]

{ #category : #enumerating }
GTEventCollectors >> do: oneArgBlock [
	mutex critical: [ collectors do: oneArgBlock ]
]

{ #category : #'gt-inspector-extension' }
GTEventCollectors >> gtInspectorItemsIn: composite [
	<gtInspectorPresentationOrder: 1>
	^ collectors ifNotNil: [ 
			(collectors gtInspectorItemsIn: composite)
				title: 'Collectors' ]
]

{ #category : #initialization }
GTEventCollectors >> initialize [
	super initialize.
	mutex := Mutex new.
	collectors := OrderedCollection new.
]

{ #category : #accessing }
GTEventCollectors >> numberOfEvents [
	^ mutex critical: [ 
		collectors 
			ifEmpty: 0
			ifNotEmpty: [ collectors sum: #size ] ]
]

{ #category : #printing }
GTEventCollectors >> printOn: aStream [
	super printOn: aStream.
	aStream
		space;
		nextPut: $[;
		print: self size;
		space;
		nextPutAll: ('collector' asPluralBasedOn: self size);
		nextPutAll: ', ';
		print: self numberOfEvents;
		space;
		nextPutAll: ('event' asPluralBasedOn: self numberOfEvents);
		nextPut: $]
]

{ #category : #removing }
GTEventCollectors >> remove: aGTEventCollector [ 
	^ mutex critical: [ collectors remove: aGTEventCollector ifAbsent: nil ]
]

{ #category : #removing }
GTEventCollectors >> removeLiberatedCollectors [
	mutex critical: [ | liberated |
		liberated := collectors select: #isLiberated.
		collectors removeAll: liberated ]
]

{ #category : #accessing }
GTEventCollectors >> size [
	^ mutex critical: [ collectors size ]
]
