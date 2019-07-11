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

{ #category : #accessing }
MethodNameEditorTest >> a [ 
	 
]

{ #category : #accessing }
MethodNameEditorTest >> a: a [
	 
]

{ #category : #accessing }
MethodNameEditorTest >> a: a b: b c: c [
	 
]

{ #category : #running }
MethodNameEditorTest >> methodNamedFor: aSymbol [
	^ RBMethodName selector: (self class >> aSymbol) selector arguments: ((self class >> aSymbol) ast arguments collect: #name)
]

{ #category : #running }
MethodNameEditorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	super setUp.
	presenterMNEThreeSetter := SycMethodNameEditor on: (self methodNamedFor: #a:b:c:).

	presenterMNEOneSetter := SycMethodNameEditor on: (self methodNamedFor: #a:).

	presenterMNEOneGetter := SycMethodNameEditor on: (self methodNamedFor: #a)
]

{ #category : #running }
MethodNameEditorTest >> tearDown [
	presenterMNEOneGetter ifNotNil: [ :x | x application close ].
	presenterMNEOneSetter ifNotNil: [ :x | x application close ].
	presenterMNEThreeSetter ifNotNil: [ :x | x application close ].
	super tearDown
]

{ #category : #tests }
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

{ #category : #tests }
MethodNameEditorTest >> testButtonDownClickShouldModifiedPreviewFinal [
	presenterMNEThreeSetter argumentsList selectIndex: 1.
	presenterMNEThreeSetter downButton click.
	self
		assert: presenterMNEThreeSetter previewResult label
		equals: 'a: b b: a c: c'
]

{ #category : #tests }
MethodNameEditorTest >> testButtonDownClickShouldSelectionIndexBeDecremented [
	presenterMNEThreeSetter argumentsList selectIndex: 1.
	presenterMNEThreeSetter downButton click.
	self
		assert: presenterMNEThreeSetter argumentsList selection selectedIndex
		equals: 2
]

{ #category : #tests }
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

{ #category : #tests }
MethodNameEditorTest >> testButtonUpClickShouldModifiedPreviewFinal [
	presenterMNEThreeSetter argumentsList selectIndex: 2.
	presenterMNEThreeSetter upButton click.
	self assert: presenterMNEThreeSetter previewResult label equals: 'a: b b: a c: c'
]

{ #category : #tests }
MethodNameEditorTest >> testButtonUpClickShouldSelectionIndexBeDecremented [
	presenterMNEThreeSetter argumentsList selectIndex: 2.
	presenterMNEThreeSetter upButton  click.
	self
		assert: presenterMNEThreeSetter argumentsList selection selectedIndex
		equals: 1
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorShouldChangePreviewLabelFinal [
	presenterMNEThreeSetter selectorInput text: 'd:e:f:'.
	self assert: presenterMNEThreeSetter previewResult label  equals: 'd: a e: b f: c'
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorWithFewAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	presenterMNEThreeSetter selectorInput text: 'd:'.
	self assert: presenterMNEThreeSetter previewResult label equals: '(invalid)'
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorWithNothingShouldChangePreviewLabelFinalWithFailedText [
	presenterMNEThreeSetter selectorInput text: ''.
	self assert: presenterMNEThreeSetter previewResult label equals: '(invalid)'
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorWithToMuchAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	presenterMNEThreeSetter selectorInput text: 'd:e:f:g:'.
	self assert: presenterMNEThreeSetter previewResult label equals: '(invalid)'
]

{ #category : #tests }
MethodNameEditorTest >> testInitializationButtonDownIsDisable [
	self deny: presenterMNEOneGetter downButton isEnabled
]

{ #category : #tests }
MethodNameEditorTest >> testInitializationButtonUpIsDisable [
	self deny:  presenterMNEOneGetter upButton isEnabled
]

{ #category : #tests }
MethodNameEditorTest >> testInitializationSelectedArgumentsIsNil [
	self assert:  presenterMNEOneGetter argumentsList selection selectedItem equals: nil
]
