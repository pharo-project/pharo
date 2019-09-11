Class {
	#name : #ConfigurableHistoryIteratorTest,
	#superclass : #TestCase,
	#instVars : [
		'historyList',
		'saved'
	],
	#category : #'System-History-Tests'
}

{ #category : #running }
ConfigurableHistoryIteratorTest >> setUp [
	super setUp.
	historyList := ConfigurableHistoryIterator undo: [ :integer | saved := integer ] redo: [ :integer | saved := integer asString ]
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testHasNext [
	historyList register: 3.

	self deny: historyList hasNext.

	historyList undo.

	self assert: historyList hasNext
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testHasPrevious [
	historyList register: 3.

	self assert: historyList hasPrevious.

	historyList undo.

	self deny: historyList hasPrevious
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testRedo [
	historyList register: 3.
	historyList register: 5.

	historyList undo.
	historyList undo.

	self assert: historyList hasNext.

	historyList redo.

	self assert: historyList hasNext.
	self assert: saved equals: '3'.
	self assert: historyList current equals: 3.
	
	historyList redo.

	self deny: historyList hasNext.
	self assert: saved equals: '5'.
	self assert: historyList current equals: 5.
	
	self should: [ historyList redo ] raise: NothingToRedo
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testRedoIfEmpty [
	historyList register: 3.

	historyList undo.

	self assert: historyList hasNext.

	historyList redoIfEmpty: [ saved := nil ].

	self deny: historyList hasNext.
	self assert: saved equals: '3'.
	
	historyList redoIfEmpty: [ saved := nil ].

	self assert: saved isNil
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testRegister [
	historyList register: 3.
	self assert: historyList size equals: 1.
	self assert: historyList current identicalTo: 3.

	historyList register: 5.
	self assert: historyList size equals: 2.
	self assert: historyList current identicalTo: 5
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testRegisterClearRedo [
	historyList register: 3.
	historyList register: 5.

	historyList undo.

	self assert: historyList size equals: 1.
	self assert: historyList hasNext.

	historyList register: 4.

	self assert: historyList size equals: 2.
	self deny: historyList hasNext
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testUndo [
	historyList register: 3.
	historyList register: 5.

	self assert: historyList hasPrevious.
	self assert: historyList current identicalTo: 5.

	historyList undo.

	self assert: historyList hasPrevious.
	self assert: historyList current identicalTo: 3.
	self assert: saved equals: 5.

	historyList undo.

	self deny: historyList hasPrevious.
	self should: [ historyList current ] raise: NothingToUndo.
	self assert: saved equals: 3.
	self should: [ historyList undo ] raise: NothingToUndo
]

{ #category : #tests }
ConfigurableHistoryIteratorTest >> testUndoIfEmpty [
	historyList register: 3.

	self assert: historyList hasPrevious.
	self assert: historyList current identicalTo: 3.

	historyList undoIfEmpty: [ saved := nil ].

	self deny: historyList hasPrevious.
	self assert: saved equals: 3.

	historyList undoIfEmpty: [ saved := nil ].

	self assert: saved isNil
]
