"
Description
--------------------

I am a demo page to show how to use spec lists.
"
Class {
	#name : #SpListsDemoPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Lists'
}

{ #category : #initialization }
SpListsDemoPage class >> pageName [
	^ 'Lists'
]

{ #category : #initialization }
SpListsDemoPage >> pageClass [
	^ SpDemoListsPresenter
]
