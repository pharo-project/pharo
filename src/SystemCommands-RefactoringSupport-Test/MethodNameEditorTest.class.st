"
this class test MethodNameEditor 
	
	I group method in Tag/Protocol:
		* action : all method in kind of Tag/Protocol are test of interacting with the widget for example (what will append if i click on button ...)
		* initialize-test : in this Tag/Protocol there are methods who test the initial configuration of a widget 
	
	
"
Class {
	#name : #MethodNameEditorTest,
	#superclass : #TestCase,
	#instVars : [
		'presenterMNEThreeSetter',
		'presenterMNEOneSetter',
		'presenterMNEOneGetter'
	],
	#classInstVars : [
		'methodNameEditorWithOneGetter'
	],
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #running }
MethodNameEditorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	"Clement tu dois finir d'init les MethodNameEditor et pense a poser une question a esteban "

	super setUp.
	presenterMNEThreeSetter := SycMethodNameEditor
		on:
			(RBMethodName
				selector: (UseOnlyForTest >> #a:b:c:) selector
				arguments: ((UseOnlyForTest >> #a:b:c:) ast arguments collect: #name)).
	presenterMNEOneSetter := SycMethodNameEditor
		on:
			(RBMethodName
				selector: (UseOnlyForTest >> #a:) selector
				arguments: ((UseOnlyForTest >> #a:) ast arguments collect: #name)).
	presenterMNEOneGetter := SycMethodNameEditor
		on:
			(RBMethodName
				selector: (UseOnlyForTest >> #a) selector
				arguments:( (UseOnlyForTest >> #a) ast arguments collect: #name)).
]

{ #category : #'action-input' }
MethodNameEditorTest >> tearDown [
	presenterMNEOneGetter ifNotNil: [ :x | x application close ].
	presenterMNEOneSetter ifNotNil: [ :x | x application close ].
	presenterMNEThreeSetter ifNotNil: [ :x | x application close ].
	super tearDown
]

{ #category : #'action-accept' }
MethodNameEditorTest >> testButtonAcceptClickShouldSetcancelValueToFalse [
	self flag: #TODO.
	"i can't test it "
]

{ #category : #'action-cancel' }
MethodNameEditorTest >> testButtonCancelClickShouldCloseWindow [
	self flag: #DONTKNOWN.
	"i can't test it"
]

{ #category : #'action-button-down' }
MethodNameEditorTest >> testButtonDownClickShouldModifiedArgumentsList [
	| arrayBeforeClick arrayAfterClick elt1 elt2 |
	presenterMNEThreeSetter argumentsList selectIndex: 2.
	arrayBeforeClick := presenterMNEThreeSetter argumentsList items.
	presenterMNEThreeSetter downButton click.
	arrayAfterClick := presenterMNEThreeSetter argumentsList items.
	elt2 := arrayAfterClick at: 2.
	elt1 := arrayAfterClick at: 1.
	self
		assert: arrayBeforeClick
		equals:
			(arrayAfterClick
				at: 1 put: elt2;
				at: 2 put: elt1;
				yourself)
]

{ #category : #'action-button-down' }
MethodNameEditorTest >> testButtonDownClickShouldModifiedPreviewFinal [
	presenterMNEThreeSetter argumentsList selectIndex: 1.
	presenterMNEThreeSetter downButton click.
	self
		assert: presenterMNEThreeSetter previewResult label
		equals: 'a: B b: A c: C'
]

{ #category : #'action-button-down' }
MethodNameEditorTest >> testButtonDownClickShouldSelectionIndexBeDecremented [
	presenterMNEThreeSetter argumentsList selectIndex: 1.
	presenterMNEThreeSetter downButton click.
	self
		assert: presenterMNEThreeSetter argumentsList selection selectedIndex
		equals: 2
]

{ #category : #'action-button-up' }
MethodNameEditorTest >> testButtonUpClickShouldModifiedArgumentsList [
	| arrayBeforeClick arrayAfterClick elt1 elt2 |
	presenterMNEThreeSetter argumentsList selectIndex: 2.
	arrayBeforeClick := presenterMNEThreeSetter argumentsList items.
	presenterMNEThreeSetter downButton click.
	arrayAfterClick := presenterMNEThreeSetter argumentsList items.
	elt2 := arrayAfterClick at: 2.
	elt1 := arrayAfterClick at: 1.
	self
		assert: arrayBeforeClick
		equals:
			(arrayAfterClick
				at: 1 put: elt2;
				at: 2 put: elt1;
				yourself)
]

{ #category : #'action-button-up' }
MethodNameEditorTest >> testButtonUpClickShouldModifiedPreviewFinal [
	presenterMNEThreeSetter argumentsList selectIndex: 2.
	presenterMNEThreeSetter upButton click.
	self
		assert: presenterMNEThreeSetter previewResult label
		equals: 'a: B b: A c: C'
]

{ #category : #'action-button-up' }
MethodNameEditorTest >> testButtonUpClickShouldSelectionIndexBeDecremented [
	presenterMNEThreeSetter argumentsList selectIndex: 2.
	presenterMNEThreeSetter upButton  click.
	self
		assert: presenterMNEThreeSetter argumentsList selection selectedIndex
		equals: 1
]

{ #category : #'action-input' }
MethodNameEditorTest >> testChangeInInputSelectorShouldChangePreviewLabelFinal [
	presenterMNEThreeSetter selectorInput text: 'd:e:f:'.
	self assert: presenterMNEThreeSetter previewResult label  equals: 'd: A e: B f: C'
]

{ #category : #'action-input' }
MethodNameEditorTest >> testChangeInInputSelectorWithFewAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	presenterMNEThreeSetter selectorInput text: 'd:'.
	self assert: presenterMNEThreeSetter previewResult label equals: '(invalid)'
]

{ #category : #'action-input' }
MethodNameEditorTest >> testChangeInInputSelectorWithNothingShouldChangePreviewLabelFinalWithFailedText [
	presenterMNEThreeSetter selectorInput text: ''.
	self assert: presenterMNEThreeSetter previewResult label equals: '(invalid)'
]

{ #category : #'action-input' }
MethodNameEditorTest >> testChangeInInputSelectorWithToMuchAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	presenterMNEThreeSetter selectorInput text: 'd:e:f:g:'.
	self assert: presenterMNEThreeSetter previewResult label equals: '(invalid)'
]

{ #category : #'initialize-test' }
MethodNameEditorTest >> testInitialButtonDownIsDisable [
	""

	self deny: presenterMNEOneGetter downButton isEnabled
]

{ #category : #'initialize-test' }
MethodNameEditorTest >> testInitialButtonUpIsDisable [
	""
	self deny:  presenterMNEOneGetter upButton isEnabled
]

{ #category : #'initialize-test' }
MethodNameEditorTest >> testInitialSelectedArgumentsIsNil [
	"test if there nothing item selected in the argumentsList (a ListPresenter) at the opening of the Window "
	self assert:  presenterMNEOneGetter argumentsList selection selectedItem equals: nil
]
