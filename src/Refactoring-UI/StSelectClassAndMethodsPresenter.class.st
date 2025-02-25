Class {
	#name : 'StSelectClassAndMethodsPresenter',
	#superclass : 'StAbstractSelectionPresenter',
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'examples' }
StSelectClassAndMethodsPresenter class >> example [
	<script>
	^ (self label: 'example ...'
		dropLabel: 'Pull up methods'
		withItems: self class methods
		selecting: OrderedCollection new
		dropItems: self class allSubclasses
		acceptBlock: [ :item :items |  ] )
]

{ #category : 'specs' }
StSelectClassAndMethodsPresenter class >> title [
	^ 'Methods'
]

{ #category : 'attributes' }
StSelectClassAndMethodsPresenter >> columnName [
	^ 'Methods'
]

{ #category : 'attributes' }
StSelectClassAndMethodsPresenter >> columnSelector [
	^ #selector
]
