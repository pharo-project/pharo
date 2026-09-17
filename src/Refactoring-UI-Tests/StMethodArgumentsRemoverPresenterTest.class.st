"
I am a test for StMethodArgumentsRemoverPresenter.

I check that the first argument of a keyword method, which carries the method name, can only be removed when it is the sole argument (the method then becomes unary).
"
Class {
	#name : 'StMethodArgumentsRemoverPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'removerPresenter'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'helpers' }
StMethodArgumentsRemoverPresenterTest >> presenterFor: aSelector arguments: aCollection [

	^ StMethodArgum


]

{ #category : 'running' }
StMethodArgumentsRemoverPresenterTest >> setUp [

	super setUp.
	removerPresenter := self presenterFor: #a:b:c: arguments: #( 'a' 'b' 'c' )
]

{ #category : 'running' }
StMethodArgumentsRemoverPresenterTest >> tearDown [

	removerPresenter ifNotNil: [ :presenter | presenter application closeAllWindows ].
	super tearDown
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testAcceptingWithFirstArgumentSelectedIsRefused [

	| presenter application |
	presenter := self presenterFor: #a:b:c: arguments: #( 'a' 'b' 'c' ).
	application := StApplicationForRefactorDriverMock new.
	presenter application: application.
	presenter argumentsList selectIndex: 1.
	presenter renameMethodAndClose: nil.
	self assert: application shownMessage equals: 'The first argument (method name) cannot be removed.'
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testDefaultSelectionIsOnlyArgumentForSingleArgumentMethod [

	| presenter |
	presenter := self presenterFor: #aWithAnObject: arguments: #( 'anObject' ).
	self assert: presenter argumentsList selection selectedIndex equals: 1
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testDefaultSelectionSkipsFirstArgumentForKeywordMethod [

	self assert: removerPresenter argumentsList selection selectedIndex equals: 2
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testFirstArgumentCannotBeRemovedForKeywordMethod [

	self deny: (removerPresenter canRemoveArgumentAt: 1).
	self assert: (removerPresenter canRemoveArgumentAt: 2).
	self assert: (removerPresenter canRemoveArgumentAt: 3)
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testOnlyArgumentCanBeRemovedForSingleArgumentMethod [

	| presenter |
	presenter := self presenterFor: #aWithAnObject: arguments: #( 'anObject' ).
	self assert: (presenter canRemoveArgumentAt: 1)
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testPreviewIsInvalidWhenFirstArgumentSelected [

	removerPresenter argumentsList selectIndex: 1.
	self assert: removerPresenter previewResult label equals: '(invalid)'
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testPreviewShowsRemovalOfDefaultSelection [

	self assert: removerPresenter previewResult label equals: 'a: a c: c'
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testRemovingOnlyArgumentProducesUnarySelector [

	 | presenter |
	 presenter := self presenterFor: #aWithAnObject: arguments: #( 'anObject' ).
	 self assert: (presenter defaultSelectorAfterRemoving: 'anObject') equals: #aWithAnObject
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testRemovingSecondArgumentKeepsOtherArguments [

	             removerPresenter argumentsList selectIndex: 2.
	             self assert: removerPresenter getParametersOrder asArray equals: #( 'a' 'c' )
]

{ #category : 'tests' }
StMethodArgumentsRemoverPresenterTest >> testRemovingSecondArgumentProducesExpectedSelector [

	self assert: (removerPresenter defaultSelectorAfterRemoving: 'b') equals: #a:c:
]
