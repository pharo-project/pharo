Class {
	#name : #MorphicTextInputFieldAdapterTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #running }
MorphicTextInputFieldAdapterTest >> classToTest [
	^ TextInputFieldPresenter
]

{ #category : #tests }
MorphicTextInputFieldAdapterTest >> testChangeWidgetTextUpdatesPresenter [

	self widget setTextAndAccept: 'some text'.
	
	self assert: presenter text equals: 'some text'
]

{ #category : #tests }
MorphicTextInputFieldAdapterTest >> testInvisibilityIsSetInWidget [

	presenter beInvisible.
	self assert: (self widget font isKindOf: FixedFaceFont)
]

{ #category : #tests }
MorphicTextInputFieldAdapterTest >> testMaxLengthIsSetInWidget [

	presenter maxLength: 10.
	self assert: self widget maxLength equals: 10
]

{ #category : #tests }
MorphicTextInputFieldAdapterTest >> testPresenterPlaceholderIsSetInWidget [

	presenter placeholder: 'something'.
	self assert: self widget ghostText equals: 'something'
]

{ #category : #tests }
MorphicTextInputFieldAdapterTest >> testPresenterTextIsSetInWidget [

	presenter text: 'something'.
	self assert: self widget text equals: 'something'
]

{ #category : #tests }
MorphicTextInputFieldAdapterTest >> testVisibilityIsSetInWidget [

	presenter beVisible.
	self deny: (self widget font isKindOf: FixedFaceFont)
]
