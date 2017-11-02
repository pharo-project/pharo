Class {
	#name : #IconListPresenterTest,
	#superclass : #ListPresenterTest,
	#category : 'Spec-Tests-Core-Widgets'
}

{ #category : #running }
IconListPresenterTest >> classToTest [
	^ IconListPresenter
]

{ #category : #tests }
IconListPresenterTest >> testGetIconFor [
	testedInstance
		items: #(#add #back #catalog);
		icons: [ :item |  Smalltalk ui icons iconNamed: item asSymbol ].
	self assert: (testedInstance getIconFor: #add) equals: (Smalltalk ui icons iconNamed: #add)
]
