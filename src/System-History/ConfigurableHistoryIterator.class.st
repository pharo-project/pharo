"
I am an history iterator you can configure with two blocks. One to define what to do during the redo action and the second to define what to do during the undo action. 

Then you can register context objects to me and ask to undo and redo actions, I will then use my undo and redo actions with my stored context to execute it.

Examples
--------------------
	
	| saved history |
	history := self
		undo: [ :integer | saved := integer ]
		redo: [ :integer | saved := integer asString ].
	
	history hasPrevious. ""false""
	history hasNext. ""false"" 
	
	history register: 3.
	
	history current. ""3""
	history hasNext. ""false""
	history hasPrevious. ""true""
	
	history undo.
	saved. ""3""
	
	history hasPrevious. ""false""
	history undo. ""EXCEPTION: NothingToUndo""
	
	history undoIfEmpty: [ saved := nil ].
	saved. ""nil""
	
	history register: 3.
	history register: 4.
	
	history undo.
	history undo.
	
	history hasNext. ""true""
	history redo.
	saved. ""'3'""
	history redo.
	saved. ""'4'""
	history redo. ""EXCEPTION: NothingToRedo""
	
	history redoIfEmpty: [ saved := nil ].
	saved. ""nil""
	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	redoAction:		<aValuable>		A valuable to execute when we redo an action.
	redoStack:		<aStack>			A stack containing the contexts that can be redone.
	undoAction:		<aValuable>		A valuable to execute when we undo an action.
	undoStack:		<aStack>			A stack containing the contexts that can be undone.
						
	I work with a system of two stacks:
	- One stack to store contexts to undo 
	- One stack to store contexts to redo
	
	When the user wants to undo something, I'll pop the undo stack, evaluate the undo action with it and push it on the redo stack.
	When the user wants to redo something, I'll pop the redo stack, evaluate the redo action with it and push it on the undo stack.
	
	When the user add a new context on the undo stack, I flush the redo stack.

"
Class {
	#name : #ConfigurableHistoryIterator,
	#superclass : #Object,
	#instVars : [
		'undoStack',
		'redoStack',
		'undoAction',
		'redoAction'
	],
	#category : #'System-History-Iterators'
}

{ #category : #'instance creation' }
ConfigurableHistoryIterator class >> action: aBlock [
	^ self undo: aBlock redo: aBlock
]

{ #category : #'instance creation' }
ConfigurableHistoryIterator class >> undo: aBlockClosure redo: aBlockClosure2 [
	^ self new
		undoAction: aBlockClosure;
		redoAction: aBlockClosure2;
		yourself
]

{ #category : #accessing }
ConfigurableHistoryIterator >> current [
	"I return the current element of the iteration. In case there is nothing to undo, I raise an exception."

	self hasPrevious ifFalse: [ NothingToUndo signal ].
	^ undoStack top
]

{ #category : #testing }
ConfigurableHistoryIterator >> hasNext [
	"Return true if there is at least one action to redo."

	^ redoStack isNotEmpty
]

{ #category : #testing }
ConfigurableHistoryIterator >> hasPrevious [
	"Return true if there is at least one action to undo."

	^ undoStack isNotEmpty
]

{ #category : #initialization }
ConfigurableHistoryIterator >> initialize [
	super initialize.
	undoStack := Stack new.
	redoStack := Stack new
]

{ #category : #actions }
ConfigurableHistoryIterator >> redo [
	"Redo the next action, in case there is none, raise an exception."

	self redoIfEmpty: [ NothingToRedo signal ]
]

{ #category : #accessing }
ConfigurableHistoryIterator >> redoAction: anObject [
	redoAction := anObject
]

{ #category : #actions }
ConfigurableHistoryIterator >> redoIfEmpty: aBlockClosure [
	"Redo the next action and register it in the undo list. In case there is no next action, execute the block as parameter."

	| element |
	self hasNext ifFalse: [ ^ aBlockClosure value ].
	element := redoStack pop.
	redoAction value: element.
	undoStack push: element
]

{ #category : #adding }
ConfigurableHistoryIterator >> register: anObject [
	"Register an object to the undo list. This will flush the redo list."

	undoStack push: anObject.
	redoStack removeAll
]

{ #category : #accessing }
ConfigurableHistoryIterator >> size [
	"Return the size of the previous history"

	^ undoStack size
]

{ #category : #actions }
ConfigurableHistoryIterator >> undo [
	"Undo the previous action, in case there is none, raise an exception."

	self undoIfEmpty: [ NothingToUndo signal ]
]

{ #category : #accessing }
ConfigurableHistoryIterator >> undoAction: anObject [
	undoAction := anObject
]

{ #category : #actions }
ConfigurableHistoryIterator >> undoIfEmpty: aBlockClosure [
	"Redo the previous action and register it in the redo list. In case there is no previous action, execute the block as parameter."
	| element |
	self hasPrevious ifFalse: [ ^ aBlockClosure value ].
	element := undoStack pop.
	undoAction value: element.
	redoStack push: element
]
