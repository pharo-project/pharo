"
Demo page for SpecDemoCheckboxesPresenter
"
Class {
	#name : #SpDemoCheckboxesPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Checkboxes'
}

{ #category : #specs }
SpDemoCheckboxesPage class >> pageName [

	^ 'Checkboxes'
]

{ #category : #specs }
SpDemoCheckboxesPage class >> priority [

	^ 30
]

{ #category : #initialization }
SpDemoCheckboxesPage >> pageClass [

	^ SpDemoCheckboxesPresenter

]
