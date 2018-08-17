"
Demo page for SpecDemoButtonsPresenter
"
Class {
	#name : #SpecDemoButtonsPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Demo-Buttons'
}

{ #category : #specs }
SpecDemoButtonsPage class >> pageName [

	^ 'Buttons'
]

{ #category : #specs }
SpecDemoButtonsPage class >> priority [

	^ 20
]

{ #category : #initialization }
SpecDemoButtonsPage >> pageClass [

	^ SpecDemoButtonsPresenter

]
