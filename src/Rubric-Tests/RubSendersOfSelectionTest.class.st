Class {
	#name : #RubSendersOfSelectionTest,
	#superclass : #TestCase,
	#instVars : [
		'editor',
		'editingArea',
		'sourceCode',
		'ast'
	],
	#category : #'Rubric-Tests-Base'
}

{ #category : #utils }
RubSendersOfSelectionTest >> caretAfter: aNode [ 

	editingArea selectFrom: aNode stop + 1 to: aNode stop

]

{ #category : #utils }
RubSendersOfSelectionTest >> caretBefore: aNode [ 

	editingArea selectFrom: aNode start to: aNode start - 1.

]

{ #category : #utils }
RubSendersOfSelectionTest >> caretInMiddleOf: aNode [ 
	
	editingArea selectFrom: aNode start + 1 to: aNode start.

]

{ #category : #tests }
RubSendersOfSelectionTest >> method: a zkx: bsq pn: c [

	^ self aMethod: a with: bsq x:c
]

{ #category : #tests }
RubSendersOfSelectionTest >> methodX:a [

	
]

{ #category : #utils }
RubSendersOfSelectionTest >> open [

	editingArea openInWindow
]

{ #category : #running }
RubSendersOfSelectionTest >> setUp [ 

	super setUp.
	
	editingArea := 	RubEditingArea new
		beForSmalltalkCode;
		beWrapped;
		width: 400;
		yourself.
		
	editor := RubSmalltalkEditor forTextArea: editingArea.

]

{ #category : #utils }
RubSendersOfSelectionTest >> sourceCodeOf: aMethod [ 
	
	sourceCode := aMethod sourceCode.
	ast := aMethod ast.
	editingArea updateTextWith: sourceCode asText	.
]

{ #category : #tests }
RubSendersOfSelectionTest >> testCaretInHeaderAfterArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	
	self caretAfter: (ast arguments at: 1).
	self assert: editor selectedSelector equals: #method:zkx:pn:
]

{ #category : #tests }
RubSendersOfSelectionTest >> testCaretInHeaderInBeforeArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretBefore: (ast arguments at: 1).
	
	self assert: editor selectedSelector equals: #method:zkx:pn:
]

{ #category : #tests }
RubSendersOfSelectionTest >> testCaretInHeaderInBeforeArgumentWithoutSpaceName [

	self sourceCodeOf: self class >> #methodX:.
	self caretBefore: (ast arguments at: 1).
	
	self assert: editor selectedSelector equals: #methodX:
]

{ #category : #tests }
RubSendersOfSelectionTest >> testCaretInHeaderInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: (ast arguments at: 2).
	
	self assert: editor selectedSelector equals: #method:zkx:pn:
]

{ #category : #tests }
RubSendersOfSelectionTest >> testCaretInHeaderInMiddleOfSelectorHeader [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: ast.
	
	self assert: editor selectedSelector equals: #method:zkx:pn:
]
