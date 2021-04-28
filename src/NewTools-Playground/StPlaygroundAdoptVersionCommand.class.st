"
Adopt selected version on current playground. 
This command is to be used on `StPlaygroundPageVersionsPresenter`
"
Class {
	#name : #StPlaygroundAdoptVersionCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StPlaygroundAdoptVersionCommand class >> defaultDescription [

	^ 'Adopt version contents in current playground'
]

{ #category : #default }
StPlaygroundAdoptVersionCommand class >> defaultName [

	^ 'Adopt'
]

{ #category : #executing }
StPlaygroundAdoptVersionCommand >> execute [

	self context doAdoptVersion
]
