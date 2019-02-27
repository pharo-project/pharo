Class {
	#name : #IconListPresenterTest,
	#superclass : #AbstractListPresenterTest,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
IconListPresenterTest >> classToTest [
	^ IconListPresenter
]

{ #category : #tests }
IconListPresenterTest >> testGetIconFor [
	presenter
		items: #(#add #back #catalog);
		icons: [ :item |  Smalltalk ui icons iconNamed: item asSymbol ].
	self assert: (presenter getIconFor: #add) equals: (Smalltalk ui icons iconNamed: #add)
]
