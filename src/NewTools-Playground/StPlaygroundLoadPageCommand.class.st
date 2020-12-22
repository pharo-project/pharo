"
Load selected page on current playground. 
This command is to be used on `StPlaygroundPagesPresenter`
"
Class {
	#name : #StPlaygroundLoadPageCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StPlaygroundLoadPageCommand class >> defaultDescription [

	^ 'Load page contents in current playground'
]

{ #category : #default }
StPlaygroundLoadPageCommand class >> defaultName [

	^ 'Load'
]

{ #category : #executing }
StPlaygroundLoadPageCommand >> execute [

	self context doLoadPage
]
