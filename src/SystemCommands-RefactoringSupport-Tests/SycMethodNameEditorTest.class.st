"
This class tests MethodNameEditor 
	
	I group method in Tag/Protocol:
		* action : all method in kind of Tag/Protocol are test of interacting with the widget for example (what will append if i click on button ...)
		* initialize-test : in this Tag/Protocol there are methods who test the initial configuration of a widget 
	
	
"
Class {
	#name : #SycMethodNameEditorTest,
	#superclass : #TestCase,
	#instVars : [
		'methodNameEditor'
	],
	#classInstVars : [
		'methodNameEditorWithOneGetter'
	],
	#category : #'SystemCommands-RefactoringSupport-Tests'
}

{ #category : #accessing }
SycMethodNameEditorTest >> a [ 
	 
]

{ #category : #accessing }
SycMethodNameEditorTest >> a: a b: b c: c [
	 
]

{ #category : #running }
SycMethodNameEditorTest >> methodNamedFor: aSymbol [
	^ RBMethodName 
		selector: (self class >> aSymbol) selector 
		arguments: ((self class >> aSymbol) ast arguments collect: [:each | each name])
]

{ #category : #running }
SycMethodNameEditorTest >> setUp [
	super setUp.
	methodNameEditor := SycMethodNameEditor on: (self methodNamedFor: #a:b:c:)
]

{ #category : #running }
SycMethodNameEditorTest >> tearDown [
	methodNameEditor ifNotNil: [ :x | x application close ].
	super tearDown
]

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonDownClickShouldModifiedArgumentsList [
	| arrayBeforeClick arrayAfterClick elt1 elt2 |
	methodNameEditor argumentsList selectIndex: 2.
	arrayBeforeClick := methodNameEditor argumentsList items.
	methodNameEditor downButton click.
	arrayAfterClick := methodNameEditor argumentsList items.
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

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonDownClickShouldModifiedPreviewFinal [
	methodNameEditor argumentsList selectIndex: 1.
	methodNameEditor downButton click.
	self assert: methodNameEditor previewResult label equals: 'a: b b: a c: c'
]

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonDownClickShouldSelectionIndexBeDecremented [
	methodNameEditor argumentsList selectIndex: 1.
	methodNameEditor downButton click.
	self assert: methodNameEditor argumentsList selection selectedIndex equals: 2
]

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonUpClickShouldModifiedArgumentsList [
	methodNameEditor argumentsList selectIndex: 2.
	self assert: methodNameEditor argumentsList items asArray equals: #(a b c).
	methodNameEditor downButton click.
	self assert: methodNameEditor argumentsList items asArray equals: #(a c b)
]

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonUpClickShouldModifiedPreviewFinal [
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor upButton click.
	self assert: methodNameEditor previewResult label equals: 'a: b b: a c: c'
]

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonUpClickShouldSelectionIndexBeDecremented [
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor upButton click.
	self assert: methodNameEditor argumentsList selection selectedIndex equals: 1
]

{ #category : #tests }
SycMethodNameEditorTest >> testChangeInInputSelectorShouldChangePreviewLabelFinal [
	methodNameEditor selectorInput text: 'd:e:f:'.
	self assert: methodNameEditor previewResult label equals: 'd: a e: b f: c'
]

{ #category : #tests }
SycMethodNameEditorTest >> testChangeInInputSelectorWithFewAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: 'd:'.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : #tests }
SycMethodNameEditorTest >> testChangeInInputSelectorWithNothingShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: ''.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : #tests }
SycMethodNameEditorTest >> testChangeInInputSelectorWithToMuchAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: 'd:e:f:g:'.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testChangeSelectedArgumentKeepsEnabledEditButton [
	
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor argumentsList selectIndex: 1.

	self assert: methodNameEditor okButton isEnabled
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testChangeSelectedArgumentLoadsNewEditArgumentInput [
	
	| argument |
	argument := methodNameEditor argumentsList items at: 1.
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor argumentsList selectIndex: 1.
	
	self assert: methodNameEditor editParameterInput text equals: argument
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testDeselectArgumentDisablesArgumentInput [
	
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor argumentsList unselectAll.

	self deny: methodNameEditor editParameterInput isEnabled
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testDeselectArgumentDisablesEditButton [
	
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor argumentsList unselectAll.

	self deny: methodNameEditor okButton isEnabled
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testDeselectArgumentEmptiesArgumentInput [
	
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor argumentsList unselectAll.

	self assert: methodNameEditor editParameterInput text isEmpty
]

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testEditParameterShouldHaveGhostText [

	self assert: methodNameEditor editParameterInput placeholder equals: methodNameEditor defaultParameterPlaceholderString
]

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testInitialArgumentListHasNoSelection [
	
	self assert: methodNameEditor argumentsList selection isEmpty
]

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testInitialButtonDownIsDisable [
	methodNameEditor := SycMethodNameEditor on: (self methodNamedFor: #a).
	self deny: methodNameEditor downButton isEnabled
]

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testInitialButtonUpIsDisable [
	methodNameEditor := SycMethodNameEditor on: (self methodNamedFor: #a).
	self deny: methodNameEditor upButton isEnabled
]

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testInitialOKButtonIsNotEnabled [
	
	self deny: methodNameEditor okButton isEnabled
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testSelectArgumentEnablesArgumentInput [
	
	methodNameEditor argumentsList selectIndex: 2.

	self assert: methodNameEditor editParameterInput isEnabled
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testSelectArgumentEnablesEditButton [
	
	methodNameEditor argumentsList selectIndex: 2.

	self assert: methodNameEditor okButton isEnabled
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testSelectArgumentLoadsEditArgumentInput [
	
	| argument |
	argument := methodNameEditor argumentsList items at: 2.
	methodNameEditor argumentsList selectIndex: 2.
	
	self assert: methodNameEditor editParameterInput text equals: argument
]
