"
Load selected page in a new playground. 
This command is to be used on `StPlaygroundPagesPresenter`
"
Class {
	#name : #StPlaygroundLoadPageOnNewPlaygroundCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StPlaygroundLoadPageOnNewPlaygroundCommand class >> defaultDescription [

	^ 'Load page contents in new playground'
]

{ #category : #default }
StPlaygroundLoadPageOnNewPlaygroundCommand class >> defaultName [

	^ 'Load on new playground'
]

{ #category : #executing }
StPlaygroundLoadPageOnNewPlaygroundCommand >> execute [

	self context doLoadPageOnNewPlayground
]
