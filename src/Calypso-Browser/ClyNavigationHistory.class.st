"
I implement navigation history.
I maintain two lists: 
- redoList 
- undoList 
They include collection of ClyBrowserState instances.

To record new state send following message: 

	navigationHistory recordState: aBrowserState 

And to undo changes call: 

	navigationHistory undoNavigationOf: aBrowser 
	
To redo them call: 

	navigationHistory redoNavigationOf: aBrowser
	
When you undo last browser state it adds new item to the redo list. And otherwise: when you perform redo it adds new item to the undo list.
This logic is implemented using undoExecuting and redoExecuting flags.

I allow to ignore navigation during given block: 

	navigationHistory ignoreNavigationDuring: aBlock	

During given block execution the #recordState: method do nothing. It resets flag #waitingNewState to achive this.

You can always check that history is empty: 

	navigationHistory isEmpty

Important detail:
I am implemented in the way to not keep reference to the browser and any of query results.
So long history do not prevent query results in environment cache to be garbage collected.
 	
Internal Representation and Key Implementation Points.

    Instance Variables
	redoExecuting:		<Boolean>
	redoList:		<OrderedCollection of<ClyBrowserState>>
	undoExecuting:		<Boolean>
	undoList:		<OrderedCollection of<ClyBrowserState>>
	waitingNewState:		<Boolean>
"
Class {
	#name : #ClyNavigationHistory,
	#superclass : #Object,
	#instVars : [
		'undoList',
		'redoList',
		'undoExecuting',
		'redoExecuting',
		'waitingNewState'
	],
	#category : #'Calypso-Browser-NavigationHistory'
}

{ #category : #operations }
ClyNavigationHistory >> clear [

	undoList removeAll.
	redoList removeAll
]

{ #category : #private }
ClyNavigationHistory >> executeRedoBy: aBlock [
	redoExecuting := true.
	aBlock ensure: [ redoExecuting := false. waitingNewState := true ]
]

{ #category : #private }
ClyNavigationHistory >> executeUndoBy: aBlock [
	undoExecuting := true.
	aBlock ensure: [ undoExecuting := false. waitingNewState := true ]
]

{ #category : #operations }
ClyNavigationHistory >> ignoreNavigationDuring: aBlock [
	waitingNewState := false.
	aBlock ensure: [ waitingNewState := true ]
]

{ #category : #initialization }
ClyNavigationHistory >> initialize [
	super initialize.
	undoExecuting := false.
	redoExecuting := false.
	waitingNewState := true.
	undoList := OrderedCollection new.
	redoList := OrderedCollection new
]

{ #category : #testing }
ClyNavigationHistory >> isEmpty [
	^undoList isEmpty and: [redoList isEmpty]
]

{ #category : #operations }
ClyNavigationHistory >> recordAcrossWindowNavigationFrom: aBrowser byUndo: isUndoOperation [

	| returnState |
	returnState := ClyAccrossWindowNavigationState from: aBrowser.
	isUndoOperation 
		ifTrue: [ self executeUndoBy: [ self recordState: returnState ] ]
		ifFalse: [ self executeRedoBy: [ self recordState: returnState ] ]
]

{ #category : #operations }
ClyNavigationHistory >> recordState: aBrowserState [
	aBrowserState representsConcreteQuery ifFalse: [ ^self ]. 
	waitingNewState ifFalse: [ ^self ].
	
	undoExecuting | redoExecuting ifTrue: [ 
		"During undo or redo system can trigger multiple navigation changes.
		waitingNewState flag here is to address this case. We need to track only first change"
		waitingNewState := false ].
	
	undoExecuting 
		ifTrue: [ redoList add: aBrowserState ]
		ifFalse: [ 
			redoExecuting ifFalse: [ redoList removeAll].
			undoList add: aBrowserState ]
]

{ #category : #accessing }
ClyNavigationHistory >> redoList [
	^ redoList
]

{ #category : #accessing }
ClyNavigationHistory >> redoList: anObject [
	redoList := anObject
]

{ #category : #operations }
ClyNavigationHistory >> redoNavigationOf: aBrowser [
	redoList ifEmpty: [ ^self ].
	
	self executeRedoBy: [ 
		self restoreNavigationState: redoList removeLast of: aBrowser]
]

{ #category : #private }
ClyNavigationHistory >> restoreNavigationState: aBrowserState of: aBrowser [

	aBrowserState applyTo: aBrowser byUndo: undoExecuting
]

{ #category : #accessing }
ClyNavigationHistory >> undoList [
	^ undoList
]

{ #category : #accessing }
ClyNavigationHistory >> undoList: anObject [
	undoList := anObject
]

{ #category : #operations }
ClyNavigationHistory >> undoNavigationOf: aBrowser [
	undoList ifEmpty: [ ^self ].
	
	self executeUndoBy: [ 
		self restoreNavigationState: undoList removeLast of: aBrowser ]
]
