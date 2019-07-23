Class {
	#name : #SpActionBarPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'items'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpActionBarPresenter class >> adapterName [

	^ #ActionBarAdapter
]

{ #category : #adding }
SpActionBarPresenter >> add: aButtonPresenter [
	self startItems add: aButtonPresenter
]

{ #category : #adding }
SpActionBarPresenter >> addAll: aCollection [
	self startItems addAll: aCollection
]

{ #category : #adding }
SpActionBarPresenter >> addLast: aButtonPresenter [
	
	(items 
		at: #end
		ifAbsentPut: [ OrderedCollection new ] ) 
		add: aButtonPresenter
]

{ #category : #initialization }
SpActionBarPresenter >> initialize [

	super initialize.
	items := Dictionary new asValueHolder
]

{ #category : #api }
SpActionBarPresenter >> items [

	^ items value
]

{ #category : #adding }
SpActionBarPresenter >> startItems [
	^ items at: #start ifAbsentPut: [ OrderedCollection new ]
]
