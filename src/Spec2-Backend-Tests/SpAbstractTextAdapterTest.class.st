Class {
	#name : #SpAbstractTextAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractTextAdapterTest class >> isAbstract [
	^ self = SpAbstractTextAdapterTest
]

{ #category : #testing }
SpAbstractTextAdapterTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #tests }
SpAbstractTextAdapterTest >> testChangePlaceholderTextUpdatesPresenter [
	presenter placeholder: 'some text'.
	self assert: self adapter placeHolderText equals: 'some text'
]

{ #category : #tests }
SpAbstractTextAdapterTest >> testPresenterPlaceholderIsSetInWidget [
	presenter placeholder: 'something'.
	self assert: self adapter placeHolderText equals: 'something'
]
