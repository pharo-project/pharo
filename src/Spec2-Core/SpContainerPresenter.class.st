"
I am a model for a container. My purpose is to hold multiple subwidgets to glue them together.


I should not be used directly
"
Class {
	#name : #SpContainerPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#category : #'Spec2-Core-Support'
}

{ #category : #specs }
SpContainerPresenter class >> adapterName [

	^ #ContainerAdapter
]
