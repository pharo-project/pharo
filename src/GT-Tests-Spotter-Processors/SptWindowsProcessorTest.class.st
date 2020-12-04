Class {
	#name : #SptWindowsProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#instVars : [
		'backWindow1',
		'backWindow2',
		'topMostWindow'
	],
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptWindowsProcessorTest >> processor [

	^ SptWindowsProcessor new
]

{ #category : #running }
SptWindowsProcessorTest >> setUp [

	super setUp.
	backWindow1 := StandardWindow new title: 'backWindow1'; openInWorld; yourself.
	backWindow2 := StandardWindow new title: 'backWindow2'; openInWorld; yourself.
	topMostWindow := StandardWindow new title: 'topMostWindow'; openInWorld; yourself.		
	
]

{ #category : #running }
SptWindowsProcessorTest >> tearDown [

	{backWindow1. backWindow2. topMostWindow} 
		do:[:e | e ifNotNil: [ e close ]].

	super tearDown.	
]

{ #category : #tests }
SptWindowsProcessorTest >> testBackWindowsAreFind [

	self runForText: 'back'.
	self assertResultsIncludes: backWindow1.
	self assertResultsIncludes: backWindow2.	
]

{ #category : #tests }
SptWindowsProcessorTest >> testBackWindowsAreFindWhenUsingSubstring [

	self runForText: 'wind'.
	self assertResultsIncludes: backWindow1.
	self assertResultsIncludes: backWindow2.	
]

{ #category : #tests }
SptWindowsProcessorTest >> testBackWindowsAreShown [

	self runForText: ''.
	self assertResultsIncludes: backWindow1.
	self assertResultsIncludes: backWindow2.	
]

{ #category : #tests }
SptWindowsProcessorTest >> testEmptyQueryShowsResults [

	self runForText: ''.
	self assertResultsIncludes: backWindow1
]

{ #category : #tests }
SptWindowsProcessorTest >> testTopMostWindowIsNotShown [

	self runForText: ''.
	self denyResultsIncludes: topMostWindow
]
