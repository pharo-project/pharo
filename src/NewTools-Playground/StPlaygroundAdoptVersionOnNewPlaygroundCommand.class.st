"
Adopt selected version in a new playground. 
This command is to be used on `StPlaygroundPageVersionsPresenter`
"
Class {
	#name : #StPlaygroundAdoptVersionOnNewPlaygroundCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StPlaygroundAdoptVersionOnNewPlaygroundCommand class >> defaultDescription [

	^ 'Adopt version contents in new playground'
]

{ #category : #default }
StPlaygroundAdoptVersionOnNewPlaygroundCommand class >> defaultName [

	^ 'Adopt on new playground'
]

{ #category : #executing }
StPlaygroundAdoptVersionOnNewPlaygroundCommand >> execute [

	self context doAdoptVersionOnNewPlayground
]
