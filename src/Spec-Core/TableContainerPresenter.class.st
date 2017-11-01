"
I am a model for a container with table layout. My purpose is to hold multiple subwidgets to glue them together.


I should not be used directly

"
Class {
	#name : #TableContainerPresenter,
	#superclass : #ContainerPresenter,
	#category : 'Spec-Core-Support'
}

{ #category : #specs }
TableContainerPresenter class >> adapterName [
	^ #MorphicTableContainerAdapter
]

{ #category : #specs }
TableContainerPresenter class >> defaultSpec [
	<spec>
	^ #(MorphicTableContainerAdapter
		adapt: #(model))
]
