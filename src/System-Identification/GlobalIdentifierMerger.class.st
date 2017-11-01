"
I am able to merge dictionaries of existing (in the image) and stored (on a disk) values.

First, I identify if the image values belongs to the current computer. Then I decide to favor existing or stored values. It is importatnt to notice that I change the existing dictionary received by #existing: message. GlobalIdentifierPersistence used me when he needs to load values from the local disk.

Collaborators: GlobalIdentifierPersistence 

Public API and Key Messages

- existing:
- stored: 
- merge

Example:
	GlobalIdentifierMerger new
		existing: (Dictionary newFromPairs: #(#a 1 #b  2));
		stored: (Dictionary newFromPairs: #(#a 3 #c  4));
		merge;
		existing
 
Internal Representation and Key Implementation Points.

    Instance Variables
	existing:		<Dictionary>
	stored:		<Dictionary>

"
Class {
	#name : #GlobalIdentifierMerger,
	#superclass : #Object,
	#instVars : [
		'existing',
		'stored'
	],
	#category : 'System-Identification'
}

{ #category : #actions }
GlobalIdentifierMerger >> convertValues [
	existing 
		at: #computerUUID 
		ifPresent: [ :aValue | existing at: #computerUUID put: aValue asString ]
		ifAbsent: [ "ignore" ].
	existing 
		at: #secretUUID
		ifPresent: [ :aValue | existing at: #secretUUID put: aValue asString ]
		ifAbsent: [ "ignore" ].
]

{ #category : #accessing }
GlobalIdentifierMerger >> existing [
	^ existing
]

{ #category : #accessing }
GlobalIdentifierMerger >> existing: anObject [
	existing := anObject
]

{ #category : #actions }
GlobalIdentifierMerger >> merge [
	"If UUIDs are different, it is likely different user and we should use
	localy stored settings. If the UUIDs are same, we keep settings of the image.
	
	It returns true if existing dictionary is changed."
	^ ((existing at: #computerUUID ifAbsent: nil) = (stored at: #computerUUID ifAbsent: nil)
		or: [ (stored includesKey: #computerUUID) not ])
			ifTrue: [ self mergeSameComputer ]
			ifFalse: [ self mergeDifferentComputer ]
]

{ #category : #actions }
GlobalIdentifierMerger >> mergeDifferentComputer [
	"It puts all stored values into existing dictionary."
	stored keysAndValuesDo: [ :eachKey :eachValue |
		existing at: eachKey put: eachValue ].
	self convertValues.
	^ true
]

{ #category : #actions }
GlobalIdentifierMerger >> mergeSameComputer [
	"It add into existing dictionary keys that are included in the stored dictionary,
	but are missing in the existing dictionary. Useful when a new version
	adds new configuration values that should be saved too.
	
	It returns true if existing dictionary is changed."
	| difference |
	difference := stored keys difference: existing keys.
	difference ifEmpty: [ ^ false ].
	difference do: [ :eachKey |
		existing at: eachKey put: (stored at: eachKey) ].
	self convertValues.
	^ true
]

{ #category : #accessing }
GlobalIdentifierMerger >> stored [
	^ stored
]

{ #category : #accessing }
GlobalIdentifierMerger >> stored: anObject [
	stored := anObject
]
