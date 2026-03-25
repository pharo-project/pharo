Class {
	#name : 'StMethodsSelectionPresenter',
	#superclass : 'StItemsSelectionPresenter',
	#instVars : [
		'acceptBlock'
	],
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

{ #category : 'initialization' }
StMethodsSelectionPresenter class >> title: aTitle label: aString withItems: items selecting: selectedItems acceptBlock: aBlock [

	^ self new
		  title: aTitle
		  label: aString
		  withItems: items
		  selecting: selectedItems
		  acceptBlock: aBlock;
		  openBlockedDialog; 
		  yourself
]

{ #category : 'actions' }
StMethodsSelectionPresenter >> accept [
	acceptBlock value: table selectedItems
]

{ #category : 'attributes' }
StMethodsSelectionPresenter >> columnName [
	^ 'Methods'
]

{ #category : 'attributes' }
StMethodsSelectionPresenter >> columnSelector [
	^ #selector
]

{ #category : 'initialization' }
StMethodsSelectionPresenter >> title: aTitle label: aString withItems: coll1 selecting: coll2 acceptBlock: aBlock [

	titleHolder := aTitle.
	label label: aString.
	table
		items: coll1;
		selectItems: coll2.
	acceptBlock := aBlock
]
