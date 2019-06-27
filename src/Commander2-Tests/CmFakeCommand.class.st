Class {
	#name : #CmFakeCommand,
	#superclass : #CmCommand,
	#category : #'Commander2-Tests'
}

{ #category : #hooks }
CmFakeCommand >> canBeRun [
	^ self context = 42
]

{ #category : #accessing }
CmFakeCommand >> description [
	^ super description , ' ' , self context asString
]

{ #category : #initialization }
CmFakeCommand >> initialize [
	super initialize.
	self
		basicName: 'Fake command';
		basicDescription: 'I am a fake command for testing purposes.'
]

{ #category : #accessing }
CmFakeCommand >> name [
	^ super name , ' ' , self context asString
]
