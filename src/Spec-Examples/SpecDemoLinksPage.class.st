"
Description
--------------------

Demo page for SpecDemoLinksPresenter
"
Class {
	#name : #SpecDemoLinksPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Other'
}

{ #category : #specs }
SpecDemoLinksPage class >> pageName [

	^ 'Links'
]

{ #category : #specs }
SpecDemoLinksPage class >> priority [

	^ 15
]

{ #category : #initialization }
SpecDemoLinksPage >> pageClass [

	^ SpecDemoLinksPresenter
]
