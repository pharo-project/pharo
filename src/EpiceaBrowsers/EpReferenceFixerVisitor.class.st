"
I fix references from Epicea events to Epicea entries.
"
Class {
	#name : #EpReferenceFixerVisitor,
	#superclass : #Object,
	#traits : 'EpTEventVisitor',
	#classTraits : 'EpTEventVisitor classTrait',
	#instVars : [
		'referencesMap'
	],
	#category : 'EpiceaBrowsers-Exporter'
}

{ #category : #instanceCreation }
EpReferenceFixerVisitor class >> newWithMap: aMap [ 

	^ self basicNew
		initializeWithMap: aMap;
		yourself
]

{ #category : #initialization }
EpReferenceFixerVisitor >> initializeWithMap: aReferencesMap [

	self initialize.
	
	referencesMap := aReferencesMap
]

{ #category : #visitor }
EpReferenceFixerVisitor >> visitEvent: anEvent [
	"No transformation in the general case"
	
	^ anEvent
]

{ #category : #visitor }
EpReferenceFixerVisitor >> visitLogCommentModification: anEvent [ 
	^ anEvent copyWithEntryReferences: referencesMap values.
]

{ #category : #visitor }
EpReferenceFixerVisitor >> visitRedo: aRedoEvent [
	^ aRedoEvent copyWithEntryReferences: referencesMap values.
]

{ #category : #visitor }
EpReferenceFixerVisitor >> visitUndo: anUndoEvent [
	^ anUndoEvent copyWithEntryReferences: referencesMap values.
]
