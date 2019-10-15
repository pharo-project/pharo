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
	
	(items 
		at: #start
		ifAbsentPut: [ OrderedCollection new ] ) 
		add: aButtonPresenter
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

{ #category : #'private-traversing' }
SpActionBarPresenter >> presenters [

	^ self items values flattened
]

{ #category : #'private-traversing' }
SpActionBarPresenter >> presentersInFocusOrder [

	^ self presenters
]

{ #category : #'private-traversing' }
SpActionBarPresenter >> traverseInFocusOrderDo: aBlock excluding: excludes [

	self presentersInFocusOrder do: [ :each |
		each traverseInFocusOrderDo: aBlock excluding: excludes ]
]
