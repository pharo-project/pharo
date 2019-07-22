Class {
	#name : #SpTextInputFieldAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTestCase,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTextInputFieldAdapterTest >> classToTest [
	^ SpTextInputFieldPresenter
]

{ #category : #tests }
SpTextInputFieldAdapterTest >> testChangePlaceholderTextUpdatesPresenter [

	presenter placeholder: 'some text'.
	
	self assert: self adapter placeHolderText equals: 'some text'
]

{ #category : #tests }
SpTextInputFieldAdapterTest >> testChangeWidgetTextUpdatesPresenter [

	self adapter text: 'some text'.
	
	self assert: presenter text equals: 'some text'
]

{ #category : #tests }
SpTextInputFieldAdapterTest >> testMaxLengthIsSetInWidget [

	presenter maxLength: 10.
	self assert: self widget maxLength equals: 10
]

{ #category : #tests }
SpTextInputFieldAdapterTest >> testNotPasswordIsSetInWidget [

	presenter bePassword: false.
	self deny: self adapter isPassword
]

{ #category : #tests }
SpTextInputFieldAdapterTest >> testPasswordIsSetInWidget [

	presenter bePassword.
	self assert: self adapter isPassword
]

{ #category : #tests }
SpTextInputFieldAdapterTest >> testPresenterPlaceholderIsSetInWidget [

	presenter placeholder: 'something'.
	self assert: self adapter placeHolderText equals: 'something'
]

{ #category : #tests }
SpTextInputFieldAdapterTest >> testPresenterTextIsSetInWidget [

	presenter text: 'something'.
	self assert: self widget text equals: 'something'
]
