Class {
	#name : #DTNullPluginPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'drTest'
	],
	#category : #'DrTests-Spec'
}

{ #category : #specs }
DTNullPluginPresenter class >> defaultSpec [

	^ SpBoxLayout newHorizontal
]

{ #category : #updating }
DTNullPluginPresenter >> updateUI [
	
]
