Class {
	#name : #RBRemoveAllSendersRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #removing }
RBRemoveAllSendersRefactoring class >> model: aModel removeSendersOf: aSelector [
	^self new
		model: aModel;
		removeSendersOf: aSelector
]

{ #category : #removing }
RBRemoveAllSendersRefactoring class >> removeSendersOf: aSelector [
	^self new removeSendersOf: aSelector
]

{ #category : #transforming }
RBRemoveAllSendersRefactoring >> messagePattern [
	^ 'self ' , (self buildSelectorString: selector)
]

{ #category : #preconditions }
RBRemoveAllSendersRefactoring >> preconditions [
	^ self emptyCondition
]

{ #category : #removing }
RBRemoveAllSendersRefactoring >> removeMethod: aMethod of: node [
	self performCompositeRefactoring: (RBRemoveSenderRefactoring
		model: self model
		remove: node sourceInterval
		inMethod: aMethod selector
		forClass: aMethod methodClass)
]

{ #category : #transforming }
RBRemoveAllSendersRefactoring >> removeSelfSenders [
	| flag |
	flag := false.
	(self model allReferencesTo: selector) do: [ :e | | node index |
		index := 1.
		[ node := self selfSendIn: (e methodClass parseTreeFor: e selector). node size < index ] 
			whileFalse: [ 
				self onError: [ self removeMethod: e of: (node at: index) ]
					do: [ flag ifFalse: [ flag := true.
							self refactoringWarning: 
						'There are one or more senders that cannot be removed, <n>because their result is used or have direct references to method'' name. <n>Proceed anyway? '.].
						index := index + 1] ].
				].
]

{ #category : #removing }
RBRemoveAllSendersRefactoring >> removeSendersOf: aSelector [
	selector := aSelector.
]

{ #category : #transforming }
RBRemoveAllSendersRefactoring >> selfSendIn: aTree [
	| searcher nodes |
	nodes := OrderedCollection new.
	searcher := self parseTreeSearcher.
	searcher
		matches: self messagePattern
		do: [ :aNode :answer | nodes add: aNode ].
	searcher executeTree: aTree initialAnswer: {}.
	^ nodes
]

{ #category : #transforming }
RBRemoveAllSendersRefactoring >> transform [
	self removeSelfSenders
]
