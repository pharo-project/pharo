"
A full width bar designed to present contextual actions. 
It is expected to be displayed below the content and expand horizontally to fill the area.
This is like the small action bars at the bottom of a list in most mobile apps (or macOS), which shows small icons with actions.

"
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

{ #category : #documentation }
SpActionBarPresenter class >> documentFactoryMethodSelector [

	^ #newActionBar
]

{ #category : #api }
SpActionBarPresenter >> add: aButtonPresenter [
	"Add a button presenter to be shown at the start of the action bar (at the left)."
	
	(items 
		at: #start
		ifAbsentPut: [ OrderedCollection new ] ) 
		add: aButtonPresenter
]

{ #category : #api }
SpActionBarPresenter >> addLast: aButtonPresenter [
	"Add a button presenter to be shown at the end of the action bar (at the right)."

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
	"Answer a dictionary containing the pairs:  
		#start -> anOrderedCollection, 
		#end -> anOrderedCollection"

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
