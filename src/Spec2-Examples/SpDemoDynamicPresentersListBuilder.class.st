"
Demo page for SpecDemoDynamicPresentersListBuilder.
"
Class {
	#name : #SpDemoDynamicPresentersListBuilder,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Dynamic'
}

{ #category : #example }
SpDemoDynamicPresentersListBuilder class >> example [
	^ self new openWithSpec
]

{ #category : #specs }
SpDemoDynamicPresentersListBuilder class >> pageName [

	^ 'Dynamic presenters list builder'

]

{ #category : #specs }
SpDemoDynamicPresentersListBuilder class >> priority [
	^ 40
]

{ #category : #initialization }
SpDemoDynamicPresentersListBuilder >> pageClass [

	^ SpDemoDynamicMemoryGamePresenter

]
