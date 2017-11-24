"
A GTSpotterEventCollectorTest is a test class for testing the behavior of GTSpotterEventCollector
"
Class {
	#name : #GTSpotterEventCollectorTest,
	#superclass : #TestCase,
	#instVars : [
		'collector'
	],
	#category : #'GT-Spotter-EventRecorder-Tests'
}

{ #category : #running }
GTSpotterEventCollectorTest >> setUp [
	super setUp.
	collector := GTSpotterEventCollector new
]

{ #category : #tests }
GTSpotterEventCollectorTest >> testBundle [
	| events bundle |
	"collecting data"
	events := OrderedCollection new.
	events 
		add: GTSpotterRecorderOpenEvent new;
		add: GTSpotterRecorderShowPreviewEvent new;
		add: GTSpotterRecorderExitAnnouncementEvent new;
		add: GTSpotterRecorderOpenEvent new;
		add: GTSpotterRecorderShowPreviewEvent new;
		add: GTSpotterRecorderExitAnnouncementEvent new;
		add: GTSpotterRecorderOpenEvent new;
		add: GTSpotterRecorderShowPreviewEvent new.
	events do: [ :each | collector add: each ].
	"first delivery"
	bundle := collector bundle.
	self assert: collector postponedEvents size equals: 2.
	7 to: 8 do: [ :index |
		self assert: (events at: index) == (collector postponedEvents at: index - 6) ].
	self assert: bundle size equals: 6.
	bundle withIndexDo: [ :eachEvent :index |
		self assert: (events at: index) class = eachEvent class ].
	"second delivery"
	bundle := collector bundle.
	self assert: collector postponedEvents size equals: 2.
	7 to: 8 do: [ :index |
		self assert: (events at: index) == (collector postponedEvents at: index - 6) ].
	self assert: bundle isEmpty.
	"third delivery"
	collector add: GTSpotterRecorderExitAnnouncementEvent new.
	bundle := collector bundle.
	self assert: collector postponedEvents isNil.
	self assert: bundle size equals: 3.
	self assert: bundle first class equals: (events at: 7) class.
	self assert: bundle second class equals: (events at: 8) class.
	self assert: bundle third class equals: GTSpotterRecorderExitAnnouncementEvent.
]

{ #category : #tests }
GTSpotterEventCollectorTest >> testPostponedEvents [
	self assert: collector postponedEvents isNil
]
