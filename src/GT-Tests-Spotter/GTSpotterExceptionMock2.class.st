Class {
	#name : #GTSpotterExceptionMock2,
	#superclass : #Object,
	#category : #'GT-Tests-Spotter-Exceptions-Mocks'
}

{ #category : #mocks }
GTSpotterExceptionMock2 >> mockProcessorFor: aSpotterStep [
	<spotterOrder: 1>
	aSpotterStep listProcessor
		allCandidates: [ nil ]; " Exception while collecting candidates "
		itemName: [ :item | item asString ];
		filter: GTFilterSubstring
]
