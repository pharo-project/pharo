"
Demo page for SpecDemoFormPresenter
"
Class {
	#name : #SpDemoFormPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Forms'
}

{ #category : #specs }
SpDemoFormPage class >> pageName [

	^ 'Forms'
]

{ #category : #specs }
SpDemoFormPage class >> priority [

	^ 10
]

{ #category : #initialization }
SpDemoFormPage >> pageClass [

	^ SpDemoFormPresenter

]
