Class {
	#name : #SwappedColorDiffPresenter,
	#superclass : #DiffPresenter,
	#category : #'Iceberg-TipUI-Spec'
}

{ #category : #specs }
SwappedColorDiffPresenter class >> adapterName [

	^ #MorphicSwappedColorDiffAdapter
]

{ #category : #specs }
SwappedColorDiffPresenter class >> defaultSpec [
	<spec>
	
	^ #(#MorphicSwappedColorDiffAdapter
		adapt: #(model))
]
