Class {
	#name : #SpListPresenterHeaderTest,
	#superclass : #TestCase,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #tests }
SpListPresenterHeaderTest >> testHeaderIsHiddenWhenNoTitleIsDefined [
	| list |
	
	list := SpListPresenter new
		items: (1 to: 10);
		display: #asString;
		yourself.
		
	self deny: list hasHeaderTitle.
	[
		list openWithSpec.
		MorphicRenderLoop new doOneCycle.
		self deny: list adapter firstRowIsHeader ]
	ensure: [ 
		list window close ]
]
