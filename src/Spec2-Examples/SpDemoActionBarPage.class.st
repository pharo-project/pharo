"
Demo page for SpecDemoActionBarPresenter
"
Class {
	#name : #SpDemoActionBarPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Buttons'
}

{ #category : #specs }
SpDemoActionBarPage class >> pageName [

	^ 'ActionBar'
]

{ #category : #specs }
SpDemoActionBarPage class >> priority [
	^ 13
]

{ #category : #initialization }
SpDemoActionBarPage >> pageClass [

	^ SpDemoActionBarPresenter
]
