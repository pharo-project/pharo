"
I am a list of components (Presenters)
"
Class {
	#name : #SpComponentListPresenter,
	#superclass : #SpAbstractListPresenter,
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #specs }
SpComponentListPresenter class >> adapterName [
	^ #ComponentListAdapter
]

{ #category : #initialization }
SpComponentListPresenter >> addPresenter: aPresenter [
	
	aPresenter owner: self.
	self model add: aPresenter
]

{ #category : #private }
SpComponentListPresenter >> doActivateAtIndex: index [
	
	"Activate only if there is an item at that position"
	self presenters at: index ifAbsent: [ ^ self ].
	
	activationBlock cull: ((SpSingleSelectionMode on: self)
		basicSelectIndex: index;
		yourself)
]

{ #category : #testing }
SpComponentListPresenter >> includes: aPresenter [
 
	^ self presenters includes: aPresenter  
]

{ #category : #testing }
SpComponentListPresenter >> isEmpty [

	^ self presenters isEmpty
]

{ #category : #accessor }
SpComponentListPresenter >> items: aCollection [ 
	
	^ super items: (aCollection collect: [ :each | 
		each asPresenter
			owner: self;
			yourself ])
]

{ #category : #accessor }
SpComponentListPresenter >> presenters [

	^ self items
]

{ #category : #initialization }
SpComponentListPresenter >> presenters: aCollection [

	self items: aCollection
]

{ #category : #event }
SpComponentListPresenter >> whenPresentersChangedDo: aBlock [
	
	self whenModelChangedDo: aBlock
]
