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
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:)
]

{ #category : #running }
SycMethodNameEditorTest >> tearDown [
	methodNameEditor ifNotNil: [ :x | x application close ].
	super tearDown
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testAddArgument [
	
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canAddArgs: false.
	(methodNameEditor argumentsList contextMenu menuGroups first menuItems do: [ :e | 
		(e name = 'Add') ifTrue: [ self deny: e isVisible ] ]).
	
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canAddArgs: true.
	(methodNameEditor argumentsList contextMenu menuGroups first menuItems do: [ :e | 
		(e name = 'Add') ifTrue: [ self assert: e isVisible ] ]).
]

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonDownClickShouldModifiedArgumentsList [
	| arrayBeforeClick arrayAfterClick |
	methodNameEditor argumentsList selectIndex: 2.
	arrayBeforeClick := methodNameEditor argumentsList items.
	self assert: (arrayBeforeClick collect: [ :each | each newName ]) asArray equals: #(a b c).
	methodNameEditor downButton click.
	arrayAfterClick := methodNameEditor argumentsList items.
	self assert: (arrayAfterClick collect: [ :each | each newName ]) asArray equals: #(a c b).
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
SycMethodNameEditorTest >> testButtonDownClickWithoutSelectOption [
	| arrayBeforeClick arrayAfterClick |
	arrayBeforeClick := methodNameEditor argumentsList items.
	self assert: (arrayBeforeClick collect: [ :each | each newName ]) asArray equals: #(a b c).
	methodNameEditor downButton click.
	arrayAfterClick := methodNameEditor argumentsList items.
	self assert: (arrayAfterClick collect: [ :each | each newName ]) asArray equals: #(b a c).
]

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonUpClickShouldModifiedArgumentsList [
	methodNameEditor argumentsList selectIndex: 2.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray 
		equals: #(a b c).
	methodNameEditor downButton click.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray 
		equals: #(a c b)
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

{ #category : #'tests - up/down buttons' }
SycMethodNameEditorTest >> testButtonUpClickWithoutSelectOption [
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray 
		equals: #(a b c).
	methodNameEditor downButton click.
	self assert: (methodNameEditor argumentsList items collect: [ :each | each newName ]) asArray
		equals: #(b a c)
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

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testInitialArgumentListAutoSelection [
	
	self assert: methodNameEditor argumentsList selection selectedIndexes equals: #(1)
]

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testInitialButtonDownIsDisable [
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a).
	self deny: methodNameEditor downButton isEnabled
]

{ #category : #'tests - initial state' }
SycMethodNameEditorTest >> testInitialButtonUpIsDisable [
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a).
	self deny: methodNameEditor upButton isEnabled
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testRemoveArguments [
	
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRemoveArgs: false.
	(methodNameEditor argumentsList contextMenu menuGroups first menuItems do: [ :e | 
		(e name = 'Remove') ifTrue: [ self deny: e isVisible ] ]).
	
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRemoveArgs: true.
	(methodNameEditor argumentsList contextMenu menuGroups first menuItems do: [ :e | 
		(e name = 'Remove') ifTrue: [ self assert: e isVisible ] ]).
]

{ #category : #'tests - argument selection' }
SycMethodNameEditorTest >> testRenameArgument [
	
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRenameArgs: false.
	(methodNameEditor argumentsList contextMenu menuGroups first menuItems do: [ :e | 
		(e name = 'Rename') ifTrue: [ self deny: e isVisible ] ]).
	
	methodNameEditor := SycMethodNameEditorPresenter on: (self methodNamedFor: #a:b:c:).
	methodNameEditor canRenameArgs: true.
	(methodNameEditor argumentsList contextMenu menuGroups first menuItems do: [ :e | 
		(e name = 'Rename') ifTrue: [ self assert: e isVisible ] ]).
]
