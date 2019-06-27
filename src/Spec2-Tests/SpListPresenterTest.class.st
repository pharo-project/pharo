Class {
	#name : #SpListPresenterTest,
	#superclass : #SpAbstractListPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #'tests-header' }
SpListPresenterTest >> classToTest [
	
	^ SpListPresenter
]

{ #category : #'tests-header' }
SpListPresenterTest >> testHideHeaderTitleUnsetsTitle [

	presenter
		headerTitle: 'title';
		hideHeaderTitle.
	
	self deny: presenter hasHeaderTitle
]

{ #category : #'tests-header' }
SpListPresenterTest >> testIconFor [
	presenter
		items: #(#add #back #catalog);
		icons: [ :item | Smalltalk ui icons iconNamed: item asSymbol ].
	self assert: (presenter iconFor: #add) equals: (Smalltalk ui icons iconNamed: #add)
]

{ #category : #'tests-header' }
SpListPresenterTest >> testSetHeaderTitleHasTitle [

	presenter headerTitle: 'title'.
	
	self assert: presenter hasHeaderTitle
]

{ #category : #'tests-header' }
SpListPresenterTest >> testSetHeaderTitleSetsTitle [

	presenter headerTitle: 'title'.
	
	self assert: presenter headerTitle equals: 'title'
]

{ #category : #test }
SpListPresenterTest >> testWhenIconsChangedDo [
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
