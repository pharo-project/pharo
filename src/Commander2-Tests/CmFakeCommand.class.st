Class {
	#name : #CmFakeCommand,
	#superclass : #CmCommand,
	#category : #'Commander2-Tests'
}

{ #category : #testing }
CmFakeCommand >> canBeExecuted [
	^ self context = 42
]

{ #category : #accessing }
CmFakeCommand >> dynamicDescription [
	^ super dynamicDescription , ' ' , self context asString
]

{ #category : #accessing }
CmFakeCommand >> dynamicName [
	^ super dynamicName , ' ' , self context asString
]

{ #category : #initialization }
CmFakeCommand >> initialize [
	super initialize.
	self
		name: 'Fake command';
		description: 'I am a fake command for testing purposes.'
]
