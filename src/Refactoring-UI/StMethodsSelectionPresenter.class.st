Class {
	#name : 'StMethodsSelectionPresenter',
	#superclass : 'StItemsSelectionPresenter',
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'examples' }
StMethodsSelectionPresenter class >> example [
	<script>
	^ (self label: 'example ...'
		withItems: self class methods
		selecting: {} )
]

{ #category : 'specs' }
StMethodsSelectionPresenter class >> title [
	^ 'Methods'
]

{ #category : 'attributes' }
StMethodsSelectionPresenter >> columnName [
	^ 'Methods'
]

{ #category : 'attributes' }
StMethodsSelectionPresenter >> columnSelector [
	^ #selector
]
