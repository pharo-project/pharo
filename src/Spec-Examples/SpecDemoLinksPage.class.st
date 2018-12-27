"
Description
--------------------

Demo page for SpecDemoLinksPresenter
"
Class {
	#name : #SpecDemoLinksPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo'
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
