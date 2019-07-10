"
Description
--------------------

Demo page for SpecDemoModalPresenter
"
Class {
	#name : #SpecDemoModalsPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Modals'
}

{ #category : #specs }
SpecDemoModalsPage class >> pageName [

	^ 'Modals'
]

{ #category : #specs }
SpecDemoModalsPage class >> priority [

	^ 30
]

{ #category : #initialization }
SpecDemoModalsPage >> pageClass [

	^ SpecDemoModalPresenter

]
