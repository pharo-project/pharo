"
I am a configuration that re-run a part of the result of a previously-run configuration.
"
Class {
	#name : #DTReRunConfiguration,
	#superclass : #DTPluginConfiguration,
	#instVars : [
		'previousResult',
		'configurationToRun',
		'originalConfiguration'
	],
	#category : #'DrTests-Model'
}

{ #category : #converting }
DTReRunConfiguration >> asTestSuite [
	^ self configurationToRun asTestSuite
]

{ #category : #accessing }
DTReRunConfiguration >> configurationToRun [
	^ configurationToRun
]

{ #category : #accessing }
DTReRunConfiguration >> configurationToRun: anObject [
	configurationToRun := anObject
]

{ #category : #accessing }
DTReRunConfiguration >> items [
	^ { self testToReRun }
]

{ #category : #accessing }
DTReRunConfiguration >> originalConfiguration [
	^ originalConfiguration
]

{ #category : #accessing }
DTReRunConfiguration >> originalConfiguration: anObject [
	originalConfiguration := anObject
]

{ #category : #accessing }
DTReRunConfiguration >> previousResult [
	^ previousResult
]

{ #category : #accessing }
DTReRunConfiguration >> previousResult: anObject [
	previousResult := anObject
]
