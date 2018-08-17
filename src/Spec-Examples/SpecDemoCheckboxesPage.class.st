"
Demo page for SpecDemoCheckboxesPresenter
"
Class {
	#name : #SpecDemoCheckboxesPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Checkboxes'
}

{ #category : #specs }
SpecDemoCheckboxesPage class >> pageName [

	^ 'Checkboxes'
]

{ #category : #specs }
SpecDemoCheckboxesPage class >> priority [

	^ 30
]

{ #category : #initialization }
SpecDemoCheckboxesPage >> pageClass [

	^ SpecDemoCheckboxesPresenter

]
