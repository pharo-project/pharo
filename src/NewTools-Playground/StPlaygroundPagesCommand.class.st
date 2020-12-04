"
I open the page selector persenter and allow the user to chose a page to load.
"
Class {
	#name : #StPlaygroundPagesCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StPlaygroundPagesCommand class >> defaultDescription [

	^ 'Load a saved page'
]

{ #category : #accessing }
StPlaygroundPagesCommand class >> defaultIconName [

	^ #glamorousMore
]

{ #category : #default }
StPlaygroundPagesCommand class >> defaultName [

	^ 'Pages'
]

{ #category : #execution }
StPlaygroundPagesCommand >> execute [ 

	(StPlaygroundPagesPresenter 
		newApplication: context application
		model: context) 
		openDialogWithSpec
]
