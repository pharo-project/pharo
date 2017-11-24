"
I am an abstract superclass for some visitors that answer objects describing EpEvent instances.
"
Class {
	#name : #EpEntryContentVisitor,
	#superclass : #Object,
	#traits : 'EpTEventVisitor',
	#classTraits : 'EpTEventVisitor classTrait',
	#category : #'EpiceaBrowsers-Visitors'
}

{ #category : #visitor }
EpEntryContentVisitor >> visitEvent: aChange [
	^ self error: 'Should not be reached'.
]

{ #category : #visitor }
EpEntryContentVisitor >> visitTraitRemoved: aChange [
	^ self visitTraitChange: aChange
]
