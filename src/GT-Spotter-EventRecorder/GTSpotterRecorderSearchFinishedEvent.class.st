"
A GTSpotterRecorderSearchFinishedEvent is triggered when spotter finished a search and displayed all the results. This event stores the size of the query.

"
Class {
	#name : #GTSpotterRecorderSearchFinishedEvent,
	#superclass : #GTSpotterRecorderEvent,
	#instVars : [
		'query',
		'processors'
	],
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderSearchFinishedEvent >> accept: aVisitor [
	^ aVisitor visitSearchFinishedEvent: self
]

{ #category : #accessing }
GTSpotterRecorderSearchFinishedEvent >> categories [
	^ self processors 
			ifNotNil: [ :aCollection | aCollection select: #isString ]
			ifNil: [ #() ]
]

{ #category : #initialization }
GTSpotterRecorderSearchFinishedEvent >> fromAnnouncement: anAnnouncement [
	query := self textInfoFrom: anAnnouncement query.
	processors := (anAnnouncement processors flatCollect: [ :eachProcessor |
		Array 
			with: eachProcessor title
			with: eachProcessor allFilteredCandidates size ]) asArray
]

{ #category : #'gt-inspector-extension' }
GTSpotterRecorderSearchFinishedEvent >> gtInspectorItems [
	^ OrderedCollection new
			add: (#query -> self query);
			add: (#processors -> self processors);
			addAll: super gtInspectorItems;
			yourself
]

{ #category : #testing }
GTSpotterRecorderSearchFinishedEvent >> isSearchFinished [
	^ self query notNil
]

{ #category : #accessing }
GTSpotterRecorderSearchFinishedEvent >> items [
	^ self processors 
			ifNotNil: [ :aCollection | aCollection select: #isNumber ]
			ifNil: [ #() ]
]

{ #category : #accessing }
GTSpotterRecorderSearchFinishedEvent >> processors [
	^ processors
]

{ #category : #accessing }
GTSpotterRecorderSearchFinishedEvent >> query [
	^ query
]
