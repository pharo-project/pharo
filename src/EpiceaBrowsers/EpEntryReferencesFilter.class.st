"
This filter rejects an arbitrary collection of log entries.
"
Class {
	#name : #EpEntryReferencesFilter,
	#superclass : #EpFilter,
	#instVars : [
		'theLog',
		'rejectedEntryReferences'
	],
	#category : 'EpiceaBrowsers-Filters'
}

{ #category : #accessing }
EpEntryReferencesFilter class >> title [

	^ 'Hide selected entries'
]

{ #category : #comparing }
EpEntryReferencesFilter >> = other [

	^ self class = other class and: [ 
		self rejectedEntryReferences = other rejectedEntryReferences ]
]

{ #category : #testing }
EpEntryReferencesFilter >> accepts: anOmEntry [

	| reference |
	reference := theLog referenceTo: anOmEntry.

	^ (rejectedEntryReferences includes: reference) not
]

{ #category : #comparing }
EpEntryReferencesFilter >> hash [

	^ self class hash bitXor: self rejectedEntryReferences hash
]

{ #category : #accessing }
EpEntryReferencesFilter >> rejectedEntryReferences [
	^ rejectedEntryReferences
]

{ #category : #accessing }
EpEntryReferencesFilter >> rejectedEntryReferences: anObject [
	rejectedEntryReferences := anObject
]

{ #category : #accessing }
EpEntryReferencesFilter >> theLog [
	^ theLog
]

{ #category : #accessing }
EpEntryReferencesFilter >> theLog: anObject [
	theLog := anObject
]

{ #category : #accessing }
EpEntryReferencesFilter >> title [

	^ self class title
]
