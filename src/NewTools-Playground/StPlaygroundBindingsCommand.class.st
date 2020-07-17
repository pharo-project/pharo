Class {
	#name : #StPlaygroundBindingsCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StPlaygroundBindingsCommand class >> defaultDescription [

	^ 'Edit bindings list.'
]

{ #category : #default }
StPlaygroundBindingsCommand class >> defaultIconName [

	^ #glamorousTable
]

{ #category : #default }
StPlaygroundBindingsCommand class >> defaultName [

	^ 'Bindings'
]

{ #category : #execution }
StPlaygroundBindingsCommand >> execute [ 

	(StPlaygroundBindingsPresenter 
		newApplication: context application 
		model: context) 
		openDialogWithSpec
]
