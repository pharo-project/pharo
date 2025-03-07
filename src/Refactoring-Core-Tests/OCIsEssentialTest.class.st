"
SUnit tests for ASTProgramNode
"
Class {
	#name : 'OCIsEssentialTest',
	#superclass : 'OCParseTreeTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenFirstMessageInACascadeExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self foo; bar; end'.
	statement := ast body statements last messages first.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenItIsReceiverExpectTrue [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self foo bar'.
	statement := ast body statements last receiver.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenLastMessageInACascadeAndCascadeIsEssentialExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               a := self foo; bar; end'.
	statement := ast body statements last value messages last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenLastMessageInACascadeAndCascadeIsNotEssentialExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self foo; bar; end'.
	statement := ast body statements last value messages last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenMessageInACascadeExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self foo; bar; end'.
	statement := ast body statements last messages second.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenNodeIsArgumentExpectTrue [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self ars: self foo'.
	statement := ast body statements first arguments first.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenOnlyNodeInABlockNodeExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               [self foo]'.
	statement := ast body statements last body statements first.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenPartOfABlockNodeExpectTrue [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               [self foo]'.
	statement := ast body statements last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenPartOfAReturnStatementExpectTrue [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               ^ self foo'.
	statement := ast body statements last value.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenPartOfASequenceExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self foo. self bar. ^ 1'.
	statement := ast body statements first.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenPartOfASequenceLastNodeExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self foo. self bar'.
	statement := ast body statements last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenPartOfASequenceOneStatementOnlyExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               self foo'.
	statement := ast body statements last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenPartOfAnAssignementInBlockNodeExpectTrue [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               [ a := self foo]'.
	statement := ast body statements last body statements first value.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenTwoMessagesInACascadeExpectFalse [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               a := self foo; bar'.
	statement := ast body statements last value messages last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenValueOfAnAssignmentExpectTrue [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               |m1| m1 := self foo'.
	statement := ast body statements last value.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
OCIsEssentialTest >> testIsEssentialWhenVariableOfAnAssignmentExpectTrue [

	| ast statement |
	ast := OCParser parseMethod: 'a1
	               |m1| m1 := self foo'.
	statement := ast body statements last variable.
	self assert: statement isEssential 
]
