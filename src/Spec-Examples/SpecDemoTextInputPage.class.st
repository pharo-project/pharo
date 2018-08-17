"
Demo page for SpecDemoTextInputPresenter
"
Class {
	#name : #SpecDemoTextInputPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-TextInput'
}

{ #category : #specs }
SpecDemoTextInputPage class >> pageName [

	^ 'Text input'
]

{ #category : #specs }
SpecDemoTextInputPage class >> priority [

	^ 20
]

{ #category : #initialization }
SpecDemoTextInputPage >> pageClass [

	^ SpecDemoTextInputPresenter

]
