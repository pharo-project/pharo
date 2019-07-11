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
		'methodNameEditor'
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
MethodNameEditorTest >> a: a b: b c: c [
	 
]

{ #category : #running }
MethodNameEditorTest >> methodNamedFor: aSymbol [
	^ RBMethodName selector: (self class >> aSymbol) selector arguments: ((self class >> aSymbol) ast arguments collect: #name)
]

{ #category : #running }
MethodNameEditorTest >> setUp [
	super setUp.
	methodNameEditor := SycMethodNameEditor on: (self methodNamedFor: #a:b:c:)
]

{ #category : #running }
MethodNameEditorTest >> tearDown [
	methodNameEditor ifNotNil: [ :x | x application close ].
	super tearDown
]

{ #category : #tests }
MethodNameEditorTest >> testButtonDownClickShouldModifiedArgumentsList [
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

{ #category : #tests }
MethodNameEditorTest >> testButtonDownClickShouldModifiedPreviewFinal [
	methodNameEditor argumentsList selectIndex: 1.
	methodNameEditor downButton click.
	self assert: methodNameEditor previewResult label equals: 'a: b b: a c: c'
]

{ #category : #tests }
MethodNameEditorTest >> testButtonDownClickShouldSelectionIndexBeDecremented [
	methodNameEditor argumentsList selectIndex: 1.
	methodNameEditor downButton click.
	self assert: methodNameEditor argumentsList selection selectedIndex equals: 2
]

{ #category : #tests }
MethodNameEditorTest >> testButtonUpClickShouldModifiedArgumentsList [
	methodNameEditor argumentsList selectIndex: 2.
	self assert: methodNameEditor argumentsList items asArray equals: #(a b c).
	methodNameEditor downButton click.
	self assert: methodNameEditor argumentsList items asArray equals: #(a c b)
]

{ #category : #tests }
MethodNameEditorTest >> testButtonUpClickShouldModifiedPreviewFinal [
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor upButton click.
	self assert: methodNameEditor previewResult label equals: 'a: b b: a c: c'
]

{ #category : #tests }
MethodNameEditorTest >> testButtonUpClickShouldSelectionIndexBeDecremented [
	methodNameEditor argumentsList selectIndex: 2.
	methodNameEditor upButton click.
	self assert: methodNameEditor argumentsList selection selectedIndex equals: 1
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorShouldChangePreviewLabelFinal [
	methodNameEditor selectorInput text: 'd:e:f:'.
	self assert: methodNameEditor previewResult label equals: 'd: a e: b f: c'
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorWithFewAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: 'd:'.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorWithNothingShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: ''.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : #tests }
MethodNameEditorTest >> testChangeInInputSelectorWithToMuchAccessorThanArgumentsShouldChangePreviewLabelFinalWithFailedText [
	methodNameEditor selectorInput text: 'd:e:f:g:'.
	self assert: methodNameEditor previewResult label equals: '(invalid)'
]

{ #category : #tests }
MethodNameEditorTest >> testInitializationButtonDownIsDisable [
	methodNameEditor := SycMethodNameEditor on: (self methodNamedFor: #a).
	self deny: methodNameEditor downButton isEnabled
]

{ #category : #tests }
MethodNameEditorTest >> testInitializationButtonUpIsDisable [
	methodNameEditor := SycMethodNameEditor on: (self methodNamedFor: #a).
	self deny: methodNameEditor upButton isEnabled
]

{ #category : #tests }
MethodNameEditorTest >> testInitializationSelectedArgumentsIsNil [
	methodNameEditor := SycMethodNameEditor on: (self methodNamedFor: #a).
	self assert: methodNameEditor argumentsList selection selectedItem isNil
]
