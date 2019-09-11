"
Description
--------------------

I am a page for a labelled presenter demo
"
Class {
	#name : #SpDemoLabelledPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Forms'
}

{ #category : #initialization }
SpDemoLabelledPage class >> pageName [
	^ 'Labelled presenter'
]

{ #category : #initialization }
SpDemoLabelledPage >> pageClass [
	^ SpDemoLabelledPresenter
]
