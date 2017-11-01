"
I am a Spec model that uses a WorldMorph in an external OSWindow.

I am to be use by sending the openExternalWindowWithSpec message to ComposablePresenter. For example:

ProtocolBrowser new openExternalWindowWithSpec.

"
Class {
	#name : #OSWindowWorldMorphModel,
	#superclass : #WindowPresenter,
	#category : 'OSWindow-Core-Morphic'
}

{ #category : #'as yet unclassified' }
OSWindowWorldMorphModel class >> adapterName [

	^ #OSWindowWorldMorphAdapter
]

{ #category : #'as yet unclassified' }
OSWindowWorldMorphModel class >> defaultSpec [
	<spec>
	
	^ #(#OSWindowWorldMorphAdapter
		adapt: #(model))
]
