"
Description
--------------------

Demo page for SpecDemoImagePresenter
"
Class {
	#name : #SpDemoImagePage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Other'
}

{ #category : #initialization }
SpDemoImagePage class >> pageName [

	^ 'Images'
]

{ #category : #initialization }
SpDemoImagePage class >> priority [

	^ 16
]

{ #category : #initialization }
SpDemoImagePage >> pageClass [

	^ SpDemoImagePresenter
]
