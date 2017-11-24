Class {
	#name : #GTSpotterUIThreadStream,
	#superclass : #Object,
	#instVars : [
		'onAddedSelector',
		'onRemoveSelector',
		'receiver',
		'timestamp'
	],
	#category : #'GT-Spotter-Utils'
}

{ #category : #'adding/removing' }
GTSpotterUIThreadStream >> addObject: firstObject inProcessor: secondObject [

	self performSymbol: self onAddedSelector withArguments: { firstObject. secondObject }.
	Processor yield
]

{ #category : #'adding/removing' }
GTSpotterUIThreadStream >> addObjects: aCollection inProcessor: aProcessor [

	self performSymbol: #addAllCandidates:in: withArguments: { aCollection . aProcessor }.
	Processor yield
]

{ #category : #accessing }
GTSpotterUIThreadStream >> onAddedSelector [
	^ onAddedSelector
]

{ #category : #accessing }
GTSpotterUIThreadStream >> onAddedSelector: anObject [
	onAddedSelector := anObject
]

{ #category : #accessing }
GTSpotterUIThreadStream >> onRemoveSelector [
	^ onRemoveSelector
]

{ #category : #accessing }
GTSpotterUIThreadStream >> onRemoveSelector: anObject [
	onRemoveSelector := anObject
]

{ #category : #performing }
GTSpotterUIThreadStream >> performSymbol: aSymbol withArguments: aCollection [
	|time|
	"we save current timestamp here, so that defer block can check if it belongs to the correct task"
	time := self timestamp.
	"UI manages commands for us"
	Processor yield.
	"if timestamp is wrong just skip command"
	UIManager default defer: [ time = self timestamp ifTrue: [ self receiver perform: aSymbol withEnoughArguments: aCollection ] ]
]

{ #category : #accessing }
GTSpotterUIThreadStream >> receiver [
	^ receiver
]

{ #category : #accessing }
GTSpotterUIThreadStream >> receiver: anObject [
	receiver := anObject
]

{ #category : #'adding/removing' }
GTSpotterUIThreadStream >> removeAllCandidates [

	self performSymbol: #removeAllCandidates withArguments: { }.
	Processor yield
]

{ #category : #accessing }
GTSpotterUIThreadStream >> timestamp [
	^ timestamp
]

{ #category : #accessing }
GTSpotterUIThreadStream >> timestamp: anObject [
	timestamp := anObject
]

{ #category : #updating }
GTSpotterUIThreadStream >> updateTimestamp [

	self timestamp: Time microsecondClockValue
]
