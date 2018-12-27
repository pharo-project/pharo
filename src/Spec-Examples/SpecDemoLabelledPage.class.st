"
Description
--------------------

I am a page for a labelled presenter demo
"
Class {
	#name : #SpecDemoLabelledPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Forms'
}

{ #category : #initialization }
SpecDemoLabelledPage class >> pageName [
	^ 'Labelled presenter'
]

{ #category : #initialization }
SpecDemoLabelledPage >> pageClass [
	^ SpecDemoLabelledPresenter
]
