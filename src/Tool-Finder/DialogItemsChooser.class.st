"
Dialog for choosing from a list of items
"
Class {
	#name : #DialogItemsChooser,
	#superclass : #Object,
	#instVars : [
		'methodNameUI',
		'choicesList',
		'selection',
		'unselectedItems',
		'selectedItems',
		'resultList',
		'model',
		'title',
		'selectedItemsSetterSelector',
		'unselectedLabel',
		'selectedLabel'
	],
	#category : #'Tool-Finder'
}

{ #category : #'instance creation' }
DialogItemsChooser class >> on: aModel unselectedItems: unselectedItems selectedItems: selectedItems selectedItemsSetterSelector: anotherSelector title: aString unselectedItemsLabel: unselectedLabel selectedItemsLabel: selectedLabel [
	| instance |
	instance := self new 
					model: aModel;
					unselectedItems: unselectedItems;
					selectedItems: selectedItems;
					selectedItemsSetterSelector: anotherSelector;
					title: aString;
					unselectedLabel: unselectedLabel;
					selectedLabel: selectedLabel.
	^instance
]

{ #category : #opening }
DialogItemsChooser class >> open [
	<script>
	
	^self new open
]

{ #category : #accessing }
DialogItemsChooser >> choicesList [
	^ choicesList ifNil: [ choicesList := OrderedCollection new ]
]

{ #category : #accessing }
DialogItemsChooser >> choicesList: anObject [

	choicesList := anObject
]

{ #category : #initialization }
DialogItemsChooser >> initialize [
	super initialize.
	selection := OrderedCollection new.
	unselectedItems := OrderedCollection new.
	selectedItems := OrderedCollection new
]

{ #category : #accessing }
DialogItemsChooser >> methodNameUI [

	^ methodNameUI
]

{ #category : #accessing }
DialogItemsChooser >> methodNameUI: anObject [

	methodNameUI := anObject
]

{ #category : #accessing }
DialogItemsChooser >> model [
	^model
]

{ #category : #accessing }
DialogItemsChooser >> model: aModel [
	model := aModel
]

{ #category : #display }
DialogItemsChooser >> open [
	<script: 'self new open'>
	
	^(DialogItemsChooserUI on: self)
			title: self title;
			openInWorld
]

{ #category : #accessing }
DialogItemsChooser >> resultList [
	^ resultList
]

{ #category : #accessing }
DialogItemsChooser >> resultList: aList [
	resultList := aList
]

{ #category : #accessing }
DialogItemsChooser >> selectedItems [

	^ selectedItems
]

{ #category : #accessing }
DialogItemsChooser >> selectedItems: anObject [

	selectedItems := anObject
]

{ #category : #accessing }
DialogItemsChooser >> selectedItemsSetterSelector [
	^selectedItemsSetterSelector
]

{ #category : #accessing }
DialogItemsChooser >> selectedItemsSetterSelector: aSelector [
	selectedItemsSetterSelector := aSelector
]

{ #category : #accessing }
DialogItemsChooser >> selectedLabel [
	^selectedLabel
]

{ #category : #accessing }
DialogItemsChooser >> selectedLabel: anObject [

	selectedLabel := anObject
]

{ #category : #accessing }
DialogItemsChooser >> selection [
	^selection
]

{ #category : #accessing }
DialogItemsChooser >> sendSelection [
	^self model perform: self selectedItemsSetterSelector with: self selectedItems
]

{ #category : #accessing }
DialogItemsChooser >> title [
		^title
]

{ #category : #accessing }
DialogItemsChooser >> title: aFinderUI [
	title := aFinderUI
]

{ #category : #accessing }
DialogItemsChooser >> unselectedItems [

	^ unselectedItems
]

{ #category : #accessing }
DialogItemsChooser >> unselectedItems: anObject [

	unselectedItems := anObject
]

{ #category : #accessing }
DialogItemsChooser >> unselectedLabel [
	^unselectedLabel
]

{ #category : #accessing }
DialogItemsChooser >> unselectedLabel: anObject [

	unselectedLabel := anObject
]
