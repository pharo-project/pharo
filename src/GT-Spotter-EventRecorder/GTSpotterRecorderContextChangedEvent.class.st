"
A GTSpotterRecorderContextChangedEvent stores current query written in Spotter input field. This event stores the size of the query and wheather or not the current query is a refinment of a previous query.

Instance Variables
	text:		<String>

text
	- query writen in Spotter input field

"
Class {
	#name : #GTSpotterRecorderContextChangedEvent,
	#superclass : #GTSpotterRecorderEvent,
	#instVars : [
		'isRefinement',
		'currentTextInfo',
		'previousTextInfo'
	],
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderContextChangedEvent >> accept: aVisitor [
	^ aVisitor visitContextChangedEvent: self
]

{ #category : #accessing }
GTSpotterRecorderContextChangedEvent >> currentTextHashes [
	^ self stringHashesFor: self currentTextInfo
]

{ #category : #accessing }
GTSpotterRecorderContextChangedEvent >> currentTextInfo [
	^ currentTextInfo
]

{ #category : #accessing }
GTSpotterRecorderContextChangedEvent >> currentTextSizes [
	^ self stringSizesFor: self currentTextInfo
]

{ #category : #accessing }
GTSpotterRecorderContextChangedEvent >> currentTextSpecials [
	^ self stringSpecialsFor: self currentTextInfo
]

{ #category : #initialization }
GTSpotterRecorderContextChangedEvent >> fromAnnouncement: anAnnouncement [

	currentTextInfo := self textInfoFrom: anAnnouncement currentText.
	anAnnouncement previousText size == 0 ifTrue: [ ^ self ].
	previousTextInfo := self textInfoFrom: anAnnouncement previousText.
	isRefinement := (anAnnouncement currentText size > anAnnouncement previousText size 
		ifTrue: [ anAnnouncement currentText beginsWith: anAnnouncement previousText]
		ifFalse: [ anAnnouncement previousText beginsWith: anAnnouncement currentText ]).
]

{ #category : #'gt-inspector-extension' }
GTSpotterRecorderContextChangedEvent >> gtInspectorItems [
	^ OrderedCollection new
			add: (#isRefinement -> self isRefinement);
			add: (#currentTextInfo -> self currentTextInfo);
			add: (#previousTextInfo -> self previousTextInfo);
			addAll: super gtInspectorItems;
			yourself
]

{ #category : #testing }
GTSpotterRecorderContextChangedEvent >> hasCurrentTextSpecials [
	^ self currentTextSpecials includes: true
]

{ #category : #initialization }
GTSpotterRecorderContextChangedEvent >> initialize [
	super initialize.
	isRefinement := false.
]

{ #category : #testing }
GTSpotterRecorderContextChangedEvent >> isContextChanged [
	^ self currentTextInfo notNil
]

{ #category : #accessing }
GTSpotterRecorderContextChangedEvent >> isRefinement [
	^ isRefinement
]

{ #category : #accessing }
GTSpotterRecorderContextChangedEvent >> previousTextInfo [
	^ previousTextInfo
]
