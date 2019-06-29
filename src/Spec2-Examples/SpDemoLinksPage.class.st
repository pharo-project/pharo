"
Description
--------------------

Demo page for SpecDemoLinksPresenter
"
Class {
	#name : #SpDemoLinksPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Other'
}

{ #category : #specs }
SpDemoLinksPage class >> pageName [

	^ 'Links'
]

{ #category : #specs }
SpDemoLinksPage class >> priority [

	^ 15
]

{ #category : #initialization }
SpDemoLinksPage >> pageClass [

	^ SpDemoLinksPresenter
]
