Class {
	#name : 'StMethodNameEditorPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'methodNameEditor'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'accessing' }
StMethodNameEditorPresenterTest >> a [
]

{ #category : 'accessing' }
StMethodNameEditorPresenterTest >> a: a b: b c: c [
]

{ #category : 'accessing' }
StMethodNameEditorPresenterTest >> aWithAnObject: anObject [
]

{ #category : 'running' }
StMethodNameEditorPresenterTest >> methodNamedFor: aSymbol [
	^ RBMethodName
		selector: aSymbol
		arguments: ((self class >> aSymbol) ast arguments collect: [:each | each name])
]

{ #category : 'running' }
StMethodNameEditorPresenterTest >> setUp [
	super setUp.
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:)
]

{ #category : 'running' }
StMethodNameEditorPresenterTest >> tearDown [

	methodNameEditor ifNotNil: [ :x | x application closeAllWindows ].
	super tearDown
]

{ #category : 'tests - argument selection' }
StMethodNameEditorPresenterTest >> testAddArgument [

	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canAddArgs: false.
	(methodNameEditor argumentsList actions allCommands do: [ :e |
		(e name = 'Add') ifTrue: [ self deny: e isEnabled ] ]).

	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canAddArgs: true.
	(methodNameEditor argumentsList actions allCommands do: [ :e |
		(e name = 'Add') ifTrue: [ self assert: e isEnabled ] ])
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToKeywordSelector [
	"Adding an argument prompts for the keyword part then for the argument
	name; both appear in the preview."

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #('with' 'arg').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToKeywordSelector: (RBArgumentName name: 'arg' value: nil).
	self assert: methodNameEditor previewResult label equals: 'a: a b: b c: c with: arg'
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToKeywordSelectorBogusEmptyAfterCleaning [
	"A bogus keyword part falls back to 'with'; a bogus argument name
	falls back to 'anObject'."

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #('%%%%' '%%%%').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToKeywordSelector: (RBArgumentName name: '%%%%' value: nil).
	self assert: methodNameEditor previewResult label equals: 'a: a b: b c: c with: anObject'
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToKeywordSelectorBogusEmptyInForbiddenListAfterCleaning [
	"An argument name that cleans up to a forbidden name (self) is
	replaced by anObject."

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #('with' '%%%%self').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToKeywordSelector: (RBArgumentName name: '%%%%' value: nil).
	self assert: methodNameEditor previewResult label equals: 'a: a b: b c: c with: anObject'
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToKeywordSelectorBogusNotEmptyAfterCleaning [
	"Non-alphanumeric characters are stripped from the entered argument
	name."

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #('with' '%arg').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToKeywordSelector: (RBArgumentName name: '%arg' value: nil).
	self assert: methodNameEditor previewResult label equals: 'a: a b: b c: c with: arg'
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToKeywordSelectorInsertedAfterAnchor [
	"Both the keyword and the argument are inserted right after the anchor
	argument, keeping the selector in sync with the argument order."

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #('point' 'aPoint').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToKeywordSelector: (methodNameEditor argumentsList items at: 1).
	self assert: methodNameEditor selectorInput text equals: 'a:point:b:c:'.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray equals: #( a aPoint b c )
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToKeywordSelectorUsesEnteredKeyword [
	"The entered keyword part is used in the selector, not a hardcoded
	one."

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #('point' 'aPoint').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToKeywordSelector: (methodNameEditor argumentsList items at: 3).
	self assert: methodNameEditor selectorInput text equals: 'a:b:c:point:'.
	self assert: methodNameEditor previewResult label equals: 'a: a b: b c: c point: aPoint'
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToUnarySelector [
	"Adding an argument to a unary method only asks for the argument
	name."

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #('point').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToUnarySelector.
	self assert: methodNameEditor previewResult label equals: 'a: point'
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testAddArgumentToUnarySelectorWithBogusEmptyAfterCleaningInput [

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog answers: #(':').
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a).
	methodNameEditor requestor: dialog.
	methodNameEditor addArgumentToUnarySelector.
	self assert: methodNameEditor previewResult label equals: 'a: anObject'.
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonDownClickOnFirstArgumentIsIgnored [
	"setUp auto-selects the first argument, which is bound to the method name
	and cannot be moved."

	| arrayBeforeClick arrayAfterClick |
	arrayBeforeClick := methodNameEditor argumentsList items.
	self assert: (arrayBeforeClick collect: [ :each | each newName ]) asArray equals: #( a b c ).
	methodNameEditor downButton click.
	arrayAfterClick := methodNameEditor argumentsList items.
	self assert: (arrayAfterClick collect: [ :each | each newName ]) asArray equals: #( a b c ).
	self assert: methodNameEditor selectorInput text equals: 'a:b:c:'
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonDownClickShouldModifiedArgumentsList [
	| arrayBeforeClick arrayAfterClick |
	methodNameEditor argumentsList selectIndex: 2.
	arrayBeforeClick := methodNameEditor argumentsList items.
	self assert: (arrayBeforeClick collect: [ :each | each newName ]) asArray equals: #(a b c).
	methodNameEditor downButton click.
	arrayAfterClick := methodNameEditor argumentsList items.
	self assert: (arrayAfterClick collect: [ :each | each newName ]) asArray equals: #(a c b)
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonDownClickShouldModifiedPreviewFinal [

	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor downButton click.
	self assert: methodNameEditor previewResult label equals: 'a: a c: c b: b'
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonDownClickShouldSelectionIndexBeIncreased [

	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor downButton click.
	self assert: methodNameEditor argumentsList selection selectedIndex equals: 3
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonUpClickOnSecondArgumentIsIgnored [
	"The second argument cannot be moved to the first position, which is
	reserved for the method name."

	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor upButton click.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray equals: #( a b c ).
	self assert: methodNameEditor selectorInput text equals: 'a:b:c:'
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonUpClickShouldModifiedArgumentsList [
	methodNameEditor argumentsList selectIndex: 2.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray
		equals: #(a b c).
	methodNameEditor downButton click.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray
		equals: #(a c b)
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonUpClickShouldModifiedPreviewFinal [

	methodNameEditor argumentsList selectIndex: 3.
	methodNameEditor upButton click.
	self assert: methodNameEditor previewResult label equals: 'a: a c: c b: b'
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testButtonUpClickShouldSelectionIndexBeDecremented [

	methodNameEditor argumentsList selectIndex: 3.
	methodNameEditor upButton click.
	self assert: methodNameEditor argumentsList selection selectedIndex equals: 2
]

{ #category : 'tests' }
StMethodNameEditorPresenterTest >> testChangeInInputSelectorShouldChangePreviewLabelFinal [
	methodNameEditor selectorInput text: 'd:e:f:'.
	self assert: methodNameEditor previewResult label equals: 'd: a e: b f: c'
]

{ #category : 'tests' }
StMethodNameEditorPresenterTest >> testChangeInInputSelectorWithFewAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: 'd:'.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : 'tests' }
StMethodNameEditorPresenterTest >> testChangeInInputSelectorWithNothingShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: ''.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : 'tests' }
StMethodNameEditorPresenterTest >> testChangeInInputSelectorWithToMuchAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: 'd:e:f:g:'.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : 'tests - argument selection' }
StMethodNameEditorPresenterTest >> testGetNewArgumentName [

	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog label: 'aPoint'.
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor requestor: dialog.
	self assert: methodNameEditor getNewArgumentName equals: 'aPoint'.
]

{ #category : 'tests - argument selection' }
StMethodNameEditorPresenterTest >> testGetNewKeywordName [
	
	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog label: 'point'.
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor requestor: dialog.
	self assert: methodNameEditor getNewKeywordPartBodyName equals: 'point'
	
	
]

{ #category : 'tests - argument selection' }
StMethodNameEditorPresenterTest >> testGetNewKeywordNameMockReturnsTheValue [
	
	| dialog |
	dialog := StMethodNameEditorPresenterAnswerForTest new.
	dialog label: ':'.
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor requestor: dialog.
	self assert: methodNameEditor getNewKeywordPartBodyName equals: ':'
	
	
]

{ #category : 'tests - initial state' }
StMethodNameEditorPresenterTest >> testInitialArgumentListAutoSelection [

	self assert: methodNameEditor argumentsList selection selectedIndexes equals: #(1)
]

{ #category : 'tests - initial state' }
StMethodNameEditorPresenterTest >> testInitialButtonDownIsDisable [
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a).
	self deny: methodNameEditor downButton isEnabled
]

{ #category : 'tests - initial state' }
StMethodNameEditorPresenterTest >> testInitialButtonUpIsDisable [
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a).
	self deny: methodNameEditor upButton isEnabled
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithConflictingArgumentName [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	self assert: (methodNameEditor newArgName: 'a') equals: 'a1'.
	self assert: (methodNameEditor newArgName: 'b') equals: 'b1'
	
	
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithGarbage [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	self assert: (methodNameEditor newArgName: '!!@%%$$##!!') equals: 'anObject'
	
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithGarbageWithLetters [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	self assert: (methodNameEditor newArgName: '!!@ab!!') equals: 'ab'
	
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithGarbageWithNumber [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	self assert: (methodNameEditor newArgName: '!!@122!!') equals: 'a122'
	
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithSelf [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	self assert: (methodNameEditor newArgName: 'self') equals: 'anObject'
	
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithanObjectGiveSelfOne [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #aWithAnObject:) ast arguments collect: [:each | each name]).
	self assert: (methodNameEditor newArgName: 'self') equals: 'anObject1'
	
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithanObjectWithCleanedVersionEndingUpInForbiddenList [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #aWithAnObject:).
	methodNameEditor invalidArgNames: ((self class >> #aWithAnObject:) ast arguments collect: [:each | each name]).
	"we cannot propose anObject for the new argument as replacement of %%self because the method second argument is already anObject
		%%self -> self -> '' -> anObject -> anObject1'"
	self assert: (methodNameEditor newArgName: '%%self') equals: 'anObject1'
	
]

{ #category : 'tests - argument validation' }
StMethodNameEditorPresenterTest >> testNewArgNameWithoutConflictingArgumentName [
	
	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor invalidArgNames: ((self class >> #a:b:c:) ast arguments collect: [:each | each name]).
	self assert: (methodNameEditor newArgName: 'point') equals: 'point'
	
]

{ #category : 'tests - argument selection' }
StMethodNameEditorPresenterTest >> testRemoveArguments [

	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRemoveArgs: false.
	(methodNameEditor argumentsList actions allCommands do: [ :e |
		(e name = 'Remove') ifTrue: [ self deny: e isEnabled ] ]).

	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRemoveArgs: true.
	(methodNameEditor argumentsList actions allCommands do: [ :e |
		(e name = 'Remove') ifTrue: [ self assert: e isEnabled ] ])
]

{ #category : 'tests - argument selection' }
StMethodNameEditorPresenterTest >> testRenameArgument [

	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRenameArgs: false.
	(methodNameEditor argumentsList actions allCommands do: [ :e |
		(e name = 'Rename') ifTrue: [ self deny: e isEnabled ] ]).

	methodNameEditor := StMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRenameArgs: true.
	(methodNameEditor argumentsList actions allCommands do: [ :e |
		(e name = 'Rename') ifTrue: [ self assert: e isEnabled ] ])
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testReorderDownUpdatesSelector [
	"Moving a non-leading argument down must move its keyword part as well,
	so the selector follows the argument order."

	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor downButton click.
	self assert: methodNameEditor selectorInput text equals: 'a:c:b:'.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray equals: #( a c b )
]

{ #category : 'tests - up/down buttons' }
StMethodNameEditorPresenterTest >> testReorderUpUpdatesSelector [
	"Moving a non-leading argument up must move its keyword part as well, so
	the selector follows the argument order."

	methodNameEditor argumentsList selectIndex: 3.
	methodNameEditor upButton click.
	self assert: methodNameEditor selectorInput text equals: 'a:c:b:'.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray equals: #( a c b )
]
