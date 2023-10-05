"
I am preview to select variables (Class Variable) 
"
Class {
	#name : 'StVariablesSelectionPresenter',
	#superclass : 'StItemsSelectionPresenter',
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

{ #category : 'attributes' }
StVariablesSelectionPresenter >> columnName [
	^ 'Variables'
]

{ #category : 'attributes' }
StVariablesSelectionPresenter >> columnSelector [
	^ #name
]
