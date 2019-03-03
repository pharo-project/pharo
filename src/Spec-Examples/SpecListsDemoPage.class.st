"
Description
--------------------

I am a demo page to show how to use spec litsts.
"
Class {
	#name : #SpecListsDemoPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Lists'
}

{ #category : #initialization }
SpecListsDemoPage class >> pageName [
	^ 'Lists'
]

{ #category : #initialization }
SpecListsDemoPage >> pageClass [
	^ SpecDemoListsPresenter
]
