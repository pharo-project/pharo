"
I am an history iterator you can configure with two blocks. One to define what to do during the redo action and the second to define what to do during the undo action. 

Then you can register objects to me and ask to undo and redo actions, I will then use my undo and redo actions with my stored object to execute it.

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
	redoStack:		<aStack>			A stack containing the elements that can be redone.
	undoAction:		<aValuable>		A valuable to execute when we undo an action.
	undoStack:		<aStack>			A stack containing the elements that can be undone.

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
ConfigurableHistoryIterator class >> undo: aBlockClosure redo: aBlockClosure2 [
	^ self new
		undoAction: aBlockClosure;
		redoAction: aBlockClosure2;
		yourself
]

{ #category : #accessing }
ConfigurableHistoryIterator >> current [
	self hasPrevious ifFalse: [ NothingToUndo signal ].
	^ undoStack top
]

{ #category : #testing }
ConfigurableHistoryIterator >> hasNext [
	^ redoStack isNotEmpty
]

{ #category : #testing }
ConfigurableHistoryIterator >> hasPrevious [
	^ undoStack isNotEmpty
]

{ #category : #initialization }
ConfigurableHistoryIterator >> initialize [
	super initialize.
	undoStack := Stack new.
	redoStack := Stack new
]

{ #category : #action }
ConfigurableHistoryIterator >> redo [
	self redoIfEmpty: [ NothingToRedo signal ]
]

{ #category : #accessing }
ConfigurableHistoryIterator >> redoAction: anObject [
	redoAction := anObject
]

{ #category : #action }
ConfigurableHistoryIterator >> redoIfEmpty: aBlockClosure [
	| element |
	self hasNext ifFalse: [ ^ aBlockClosure value ].
	element := redoStack pop.
	redoAction value: element.
	undoStack push: element
]

{ #category : #adding }
ConfigurableHistoryIterator >> register: anObject [
	undoStack push: anObject.
	redoStack removeAll
]

{ #category : #accessing }
ConfigurableHistoryIterator >> size [
	^ undoStack size
]

{ #category : #action }
ConfigurableHistoryIterator >> undo [
	self undoIfEmpty: [ NothingToUndo signal ]
]

{ #category : #accessing }
ConfigurableHistoryIterator >> undoAction: anObject [
	undoAction := anObject
]

{ #category : #action }
ConfigurableHistoryIterator >> undoIfEmpty: aBlockClosure [
	| element |
	self hasPrevious ifFalse: [ ^ aBlockClosure value ].
	element := undoStack pop.
	undoAction value: element.
	redoStack push: element
]
