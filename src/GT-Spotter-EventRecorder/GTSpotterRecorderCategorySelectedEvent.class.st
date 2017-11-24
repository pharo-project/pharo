"
A GTSpotterRecorderCategorySelectedEvent  models the action of  selecting a category in the search results. This event stores the name of the category and the number of results that were matched by the current query.

Instance Variables
	categoryLabel:		<Object>
	elementCount:		<Object>

categoryLabel
	- xxxxx

elementCount
	- xxxxx

"
Class {
	#name : #GTSpotterRecorderCategorySelectedEvent,
	#superclass : #GTSpotterRecorderEvent,
	#instVars : [
		'categoryLabel',
		'elementCount'
	],
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderCategorySelectedEvent >> accept: aVisitor [
	^ aVisitor visitCategorySelectedEvent: self
]

{ #category : #accessing }
GTSpotterRecorderCategorySelectedEvent >> categoryLabel [
	^ categoryLabel
]

{ #category : #accessing }
GTSpotterRecorderCategorySelectedEvent >> elementCount [
	^ elementCount
]

{ #category : #accessing }
GTSpotterRecorderCategorySelectedEvent >> elementCountOrZero [
	^ self elementCount ifNil: [ 0 ]
]

{ #category : #initialization }
GTSpotterRecorderCategorySelectedEvent >> fromAnnouncement: anAnnouncement [

	categoryLabel := anAnnouncement category title.
	elementCount := anAnnouncement category allFilteredCandidates size.
]

{ #category : #'gt-inspector-extension' }
GTSpotterRecorderCategorySelectedEvent >> gtInspectorItems [
	^ OrderedCollection new
			add: (#categoryLabel -> self categoryLabel);
			add: (#elementCount -> self elementCount);
			addAll: super gtInspectorItems;
			yourself
]

{ #category : #testing }
GTSpotterRecorderCategorySelectedEvent >> isCategorySelection [
	^ true
]
