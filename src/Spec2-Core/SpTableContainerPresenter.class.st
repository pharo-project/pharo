"
I am a model for a container with table layout. My purpose is to hold multiple subwidgets to glue them together.


I should not be used directly

"
Class {
	#name : #SpTableContainerPresenter,
	#superclass : #SpContainerPresenter,
	#category : #'Spec2-Core-Support'
}

{ #category : #specs }
SpTableContainerPresenter class >> adapterName [
	^ #TableContainerAdapter
]
