"
Demo page for SpecDemoToolBarPresenter
"
Class {
	#name : #SpDemoToolBarPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Buttons'
}

{ #category : #specs }
SpDemoToolBarPage class >> pageName [

	^ 'ToolBar'
]

{ #category : #specs }
SpDemoToolBarPage class >> priority [
	^ 12
]

{ #category : #initialization }
SpDemoToolBarPage >> pageClass [

	^ SpDemoToolBarPresenter
]
