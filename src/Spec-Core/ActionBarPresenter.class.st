Class {
	#name : #ActionBarPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'items'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
ActionBarPresenter class >> adapterName [

	^ #ActionBarAdapter
]

{ #category : #adding }
ActionBarPresenter >> add: aButtonPresenter [
	
	(items 
		at: #start
		ifAbsentPut: [ OrderedCollection new ] ) 
		add: aButtonPresenter
]

{ #category : #adding }
ActionBarPresenter >> addLast: aButtonPresenter [
	
	(items 
		at: #end
		ifAbsentPut: [ OrderedCollection new ] ) 
		add: aButtonPresenter
]

{ #category : #initialization }
ActionBarPresenter >> initialize [

	super initialize.
	items := Dictionary new asValueHolder
]

{ #category : #api }
ActionBarPresenter >> items [

	^ items value
]
