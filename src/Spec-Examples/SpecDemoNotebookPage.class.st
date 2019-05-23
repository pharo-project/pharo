Class {
	#name : #SpecDemoNotebookPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Other'
}

{ #category : #specs }
SpecDemoNotebookPage class >> pageName [
	^ 'Notebook'
]

{ #category : #specs }
SpecDemoNotebookPage class >> priority [
	^ 13
]

{ #category : #initialization }
SpecDemoNotebookPage >> pageClass [
	^ SpecDemoNotebookPresenter
]
