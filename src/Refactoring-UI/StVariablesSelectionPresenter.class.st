"
I am preview to select variables (Class Variable) 
"
Class {
	#name : 'StVariablesSelectionPresenter',
	#superclass : 'StItemsSelectionPresenter',
	#instVars : [
		'acceptBlock'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'examples' }
StVariablesSelectionPresenter class >> example [
	<script>
	^ (self label: 'example ...'
		withItems:
			{(ClassVariable named: #a) . (ClassVariable named: #b) . (ClassVariable named: #z) } asOrderedCollection
		selecting: {(ClassVariable named: #z) . (ClassVariable named: #a) } asOrderedCollection )
]

{ #category : 'specs' }
StVariablesSelectionPresenter class >> title [
	^ 'Class variables'
]

{ #category : 'initialization' }
StVariablesSelectionPresenter class >> title: aTitle label: aString withItems: items selecting: selectedItems acceptBlock: aBlock [

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
StVariablesSelectionPresenter >> accept [

	acceptBlock value: table selectedItems
]

{ #category : 'attributes' }
StVariablesSelectionPresenter >> columnName [
	^ 'Variables'
]

{ #category : 'attributes' }
StVariablesSelectionPresenter >> columnSelector [
	^ #name
]

{ #category : 'initialization' }
StVariablesSelectionPresenter >> title: aTitle label: aString withItems: coll1 selecting: coll2 acceptBlock: aBlock [

	titleHolder := aTitle.
	label label: aString.
	table
		items: coll1;
		selectItems: coll2.
	acceptBlock := aBlock
]
