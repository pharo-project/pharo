"
Demo page for SpecDemoToolBarPresenter
"
Class {
	#name : #SpDemoToolbarPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Buttons'
}

{ #category : #specs }
SpDemoToolbarPage class >> pageName [

	^ 'Toolbar'
]

{ #category : #specs }
SpDemoToolbarPage class >> priority [
	^ 12
]

{ #category : #initialization }
SpDemoToolbarPage >> pageClass [

	^ SpDemoToolbarPresenter
]
