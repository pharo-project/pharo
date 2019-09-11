"
Demo page for SpecDemoButtonsPresenter
"
Class {
	#name : #SpDemoButtonsPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Buttons'
}

{ #category : #specs }
SpDemoButtonsPage class >> pageName [

	^ 'Buttons'
]

{ #category : #specs }
SpDemoButtonsPage class >> priority [

	^ 20
]

{ #category : #initialization }
SpDemoButtonsPage >> pageClass [

	^ SpDemoButtonsPresenter

]
