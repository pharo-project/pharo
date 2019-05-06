Class {
	#name : #ListPresenterTest,
	#superclass : #AbstractListPresenterTest,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #'tests-header' }
ListPresenterTest >> classToTest [
	
	^ ListPresenter
]

{ #category : #'tests-header' }
ListPresenterTest >> testHideHeaderTitleUnsetsTitle [

	presenter
		headerTitle: 'title';
		hideHeaderTitle.
	
	self deny: presenter hasHeaderTitle
]

{ #category : #'tests-header' }
ListPresenterTest >> testIconFor [
	presenter
		items: #(#add #back #catalog);
		icons: [ :item | Smalltalk ui icons iconNamed: item asSymbol ].
	self assert: (presenter iconFor: #add) equals: (Smalltalk ui icons iconNamed: #add)
]

{ #category : #'tests-header' }
ListPresenterTest >> testSetHeaderTitleHasTitle [

	presenter headerTitle: 'title'.
	
	self assert: presenter hasHeaderTitle
]

{ #category : #'tests-header' }
ListPresenterTest >> testSetHeaderTitleSetsTitle [

	presenter headerTitle: 'title'.
	
	self assert: presenter headerTitle equals: 'title'
]

{ #category : #test }
ListPresenterTest >> testWhenIconsChangedDo [
	| icon counter |
	counter := 0.
	presenter
		whenIconsChangedDo: [ :iconBlock | 
			icon := iconBlock cull: #add.
			counter := counter + 1 ].
	presenter icons: [ :item | Smalltalk ui icons iconNamed: item asSymbol ].
	self assert: icon equals: (Smalltalk ui icons iconNamed: #add).
	self assert: counter equals: 1
]
