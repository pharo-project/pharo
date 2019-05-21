Class {
	#name : #CodePresenterTest,
	#superclass : #TextPresenterTest,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
CodePresenterTest >> classToTest [

	^ CodePresenter
]

{ #category : #tests }
CodePresenterTest >> testContextKeyBindings [

	self assert: presenter contextKeyBindings notNil
]

{ #category : #tests }
CodePresenterTest >> testContextMenu [
	| menu changed |
	
	self assert: presenter contextMenu isNotNil. "The code presenter comes with a menu by default"
	menu := MenuPresenter new.
	changed := false.
	presenter whenMenuChangedDo: [ 
		changed := true. ].
	presenter contextMenu: menu.
	self assert: presenter contextMenu equals: menu.
	self assert: changed
]
