Class {
	#name : #SpComponentListPresenterTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpComponentListPresenterTest >> classToTest [
	^ SpComponentListPresenter
]

{ #category : #tests }
SpComponentListPresenterTest >> testAddNoPresenterToComponentListDoesNotRaiseEvent [
	| raised |
	raised := false.
	presenter whenPresentersChangedDo: [ raised := true ].
	
	
	self deny: raised
]

{ #category : #tests }
SpComponentListPresenterTest >> testAddPresenterToComponentListIsInPresenterCollection [
	| button |
	button := SpButtonPresenter new.
	presenter addPresenter: button.
	self assert: (presenter includes: button)
]

{ #category : #tests }
SpComponentListPresenterTest >> testAddPresenterToComponentListRaisesEvent [
	| button raised |
	raised := false.
	button := SpButtonPresenter new.
	presenter whenPresentersChangedDo: [ raised := true ].
	
	presenter addPresenter: button.
	
	self assert: raised
]

{ #category : #tests }
SpComponentListPresenterTest >> testAddPresenterToComponentListRaisesSingleEvent [
	| button raised |
	raised := 0.
	button := SpButtonPresenter new.
	presenter whenPresentersChangedDo: [ raised := raised + 1 ].
	
	presenter addPresenter: button.
	
	self assert: raised equals: 1
]

{ #category : #tests }
SpComponentListPresenterTest >> testAddPresenterToComponentListShouldNotBeEmpty [
	presenter addPresenter: SpButtonPresenter new.
	self deny: presenter isEmpty
]

{ #category : #tests }
SpComponentListPresenterTest >> testNewComponentListIsEmpty [
	self assertEmpty: presenter 
]
