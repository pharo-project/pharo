Class {
	#name : #SpCodePresenterTest,
	#superclass : #SpTextPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpCodePresenterTest >> classToTest [

	^ SpCodePresenter
]

{ #category : #tests }
SpCodePresenterTest >> testContextKeyBindings [
	| contextKeyBindings |
	
	contextKeyBindings := presenter contextKeyBindings.
	self assert: contextKeyBindings notNil.
	self assert: (contextKeyBindings isKindOf: KMCategory).
	
]

{ #category : #tests }
SpCodePresenterTest >> testContextMenu [
	| menu changed |
	
	self assert: presenter contextMenu isNotNil. "The code presenter comes with a menu by default"
	menu := SpMenuPresenter new.
	changed := false.
	presenter whenMenuChangedDo: [ 
		changed := true. ].
	presenter contextMenu: menu.
	self assert: presenter contextMenu equals: menu.
	self assert: changed
]

{ #category : #tests }
SpCodePresenterTest >> testFindClassFrom [

	self assert: (presenter findClassFrom: '') equals: nil.
	self assert: (presenter findClassFrom: 'Object') equals: Object.
	self assert: (presenter findClassFrom: 'Object.') equals: Object.
	self assert: (presenter findClassFrom: '.Object.') equals: Object.
	self
		assert: (presenter findClassFrom: 'somethingBefore := 42.Object')
		equals: Object.
	self
		assert:
			(presenter
				findClassFrom: 'somethingBefore := 42.Object. somethingAfter := 11')
		equals: Object.
	self
		assert: (presenter findClassFrom: 'NonExistingClass.Object.')
		equals: Object
]

{ #category : #tests }
SpCodePresenterTest >> testWhenSyntaxHighlightChangedDo [
	| count result |
	count := 0.
	result := true.
	presenter
		whenSyntaxHighlightChangedDo: [ :syntaxHighlight | 
			count := count + 1.
			result := syntaxHighlight ].
	presenter withoutSyntaxHighlight.
	self assert: count equals: 1.
	self deny: result
]
