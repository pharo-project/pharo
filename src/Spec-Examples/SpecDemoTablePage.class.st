"
Description
--------------------

I am a demo showing a demo of tables.
"
Class {
	#name : #SpecDemoTablePage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Tables'
}

{ #category : #initialization }
SpecDemoTablePage class >> pageName [
	^ 'Tables'
]

{ #category : #initialization }
SpecDemoTablePage >> pageClass [
	^ SpecDemoTablePresenter
]
