"
Demo page for SpecDemoFormPresenter
"
Class {
	#name : #SpecDemoFormPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Forms'
}

{ #category : #specs }
SpecDemoFormPage class >> pageName [

	^ 'Forms'
]

{ #category : #specs }
SpecDemoFormPage class >> priority [

	^ 10
]

{ #category : #initialization }
SpecDemoFormPage >> pageClass [

	^ SpecDemoFormPresenter

]
