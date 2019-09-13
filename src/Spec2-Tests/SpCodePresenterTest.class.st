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
	self skip.
	self assert: presenter contextKeyBindings notNil
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
