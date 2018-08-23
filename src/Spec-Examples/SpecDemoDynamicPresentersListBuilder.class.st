"
Demo page for SpecDemoDynamicPresentersListBuilder.
"
Class {
	#name : #SpecDemoDynamicPresentersListBuilder,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Dynamic'
}

{ #category : #specs }
SpecDemoDynamicPresentersListBuilder class >> pageName [

	^ 'Dynamic presenters list builder'

]

{ #category : #specs }
SpecDemoDynamicPresentersListBuilder class >> priority [
	^ 40
]

{ #category : #initialization }
SpecDemoDynamicPresentersListBuilder >> pageClass [

	^ SpecDemoDynamicMemoryGamePresenter

]
