"
SUnit tests for RBProgramNode
"
Class {
	#name : #RBProgramNodeTest,
	#superclass : #TestCase,
	#instVars : [
		'node'
	],
	#category : #'AST-Tests-Core'
}

{ #category : #accessing }
RBProgramNodeTest class >> packageNamesUnderTest [
	^ #('AST-Core')
]

{ #category : #accessing }
RBProgramNodeTest >> node [
	^ node ifNil: [ node := RBProgramNode new ]
]

{ #category : #utilities }
RBProgramNodeTest >> parseExpression: aString [
	^ RBParser parseExpression: aString
]

{ #category : #utilities }
RBProgramNodeTest >> parseMethod: aString [
	^ RBParser parseMethod: aString
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddNode [
	| tree treeNode |
	tree := self parseExpression: '1. 2'.
	treeNode := tree addNode: (self parseExpression: '3').
	self assert: (self parseExpression: '1. 2. 3') equals: tree.
	self assert: tree statements last equals: treeNode.
	
	tree := self parseExpression: '{ 1. 2 }'.
	treeNode := tree addNode: (self parseExpression: '3').
	self assert: (self parseExpression: '{ 1. 2. 3 }') equals: tree.
	self assert: tree statements last equals: treeNode
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddNodeBefore [
	| tree treeNode |
	tree := self parseExpression: '1. 3'.
	treeNode := tree
		addNode: (self parseExpression: '2')
		before: tree statements last.
	self assert: (self parseExpression: '1. 2. 3') equals: tree.
	self assert: (tree statements at: 2) equals: treeNode.
	
	tree := self parseExpression: '{ 1. 3 }'.
	treeNode := tree
		addNode: (self parseExpression: '2')
		before: tree statements last.
	self assert: (self parseExpression: '{ 1. 2. 3 }') equals: tree.
	self assert: (tree statements at: 2) equals: treeNode
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddNodeFirst [
	| tree treeNode |
	tree := self parseExpression: '2. 3'.
	treeNode := tree addNodeFirst: (self parseExpression: '1').
	self assert: (self parseExpression: '1. 2. 3') equals: tree.
	self assert: tree statements first equals: treeNode.
	
	tree := self parseExpression: '{ 2. 3 }'.
	treeNode := tree addNodeFirst: (self parseExpression: '1').
	self assert: (self parseExpression: '{ 1. 2. 3 }') equals: tree.
	self assert: tree statements first equals: treeNode
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddNodes [
	| tree treeNodes |
	tree := self parseExpression: '1. 2'.
	treeNodes := tree addNodes: (self parseExpression: '3. 4') statements.
	self assert: (self parseExpression: '1. 2. 3. 4') equals: tree.
	self assert: (tree statements at: 3) equals: treeNodes first.
	self assert: (tree statements at: 4) equals: treeNodes last.
	
	tree := self parseExpression: '{ 1. 2 }'.
	treeNodes := tree addNodes: (self parseExpression: '3. 4') statements.
	self assert: (self parseExpression: '{ 1. 2. 3. 4 }') equals: tree.
	self assert: (tree statements at: 3) equals: treeNodes first.
	self assert: (tree statements at: 4) equals: treeNodes last
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddNodesBefore [
	| tree treeNodes |
	tree := self parseExpression: '1. 4'.
	treeNodes := tree 
		addNodes: (self parseExpression: '2. 3') statements
		before: tree statements last.
	self assert: (self parseExpression: '1. 2. 3. 4') equals: tree.
	self assert: (tree statements at: 2) equals: treeNodes first.
	self assert: (tree statements at: 3) equals: treeNodes last.
	
	tree := self parseExpression: '{ 1. 4 }'.
	treeNodes := tree 
		addNodes: (self parseExpression: '2. 3') statements
		before: tree statements last.
	self assert: (self parseExpression: '{ 1. 2. 3. 4 }') equals: tree.
	self assert: (tree statements at: 2) equals: treeNodes first.
	self assert: (tree statements at: 3) equals: treeNodes last
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddNodesFirst [
	| tree treeNodes |
	tree := self parseExpression: '3. 4'.
	treeNodes := tree addNodesFirst: (self parseExpression: '1. 2') statements.
	self assert: (self parseExpression: '1. 2. 3. 4') equals: tree.
	self assert: (tree statements at: 1) equals: treeNodes first.
	self assert: (tree statements at: 2) equals: treeNodes last.
	
	tree := self parseExpression: '{ 3. 4 }'.
	treeNodes := tree addNodesFirst: (self parseExpression: '1. 2') statements.
	self assert: (self parseExpression: '{ 1. 2. 3. 4 }') equals: tree.
	self assert: (tree statements at: 1) equals: treeNodes first.
	self assert: (tree statements at: 2) equals: treeNodes last
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddReturn [
	| tree return |
	tree := self parseExpression: '1. 2'.
	return := tree addReturn.
	self assert: tree statements last equals: return.
	self assert: (self parseExpression: '1. ^ 2') equals: tree.
	
	tree := self parseExpression: '3. ^ 4'.
	return := tree addReturn.
	self assert: tree statements last equals: return.
	self assert: (self parseExpression: '3. ^ 4') equals: tree
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddSelfReturn [
	| tree return |
	tree := self parseExpression: '1. 2'.
	return := tree addSelfReturn.
	self assert: tree statements last equals: return.
	self assert: (self parseExpression: '1. 2. ^ self') equals: tree.
	
	tree := self parseExpression: '3. ^ 4'.
	return := tree addSelfReturn.
	self assert: tree statements last equals: return.
	self assert: (self parseExpression: '3. ^ 4') equals: tree
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddTemporariesNamed [
	| tree variables |
	tree := self parseExpression: '| a | a'.
	variables := tree addTemporariesNamed: #('b' 'c').
	self assert: variables first isVariable.
	self assert: variables first name equals: 'b'.
	self assert: variables second isVariable.
	self assert: variables second name equals: 'c'.
	self assert: tree temporaries second equals: variables first.
	self assert: tree temporaries last equals: variables second
	
]

{ #category : #'testing-adding' }
RBProgramNodeTest >> testAddTemporaryNamed [
	| tree variable |
	tree := self parseExpression: '| a | a'.
	variable := tree addTemporaryNamed: 'b'.
	self assert: variable isVariable.
	self assert: variable name equals: 'b'.
	self assert: tree temporaries last equals: variable
]

{ #category : #'testing-properties' }
RBProgramNodeTest >> testHasProperty [
	self deny: (self node hasProperty: #foo).
	self node propertyAt: #foo put: 123.
	self assert: (self node hasProperty: #foo)
]

{ #category : #'testing-properties' }
RBProgramNodeTest >> testPropertyAt [
	self should: [ self node propertyAt: #foo ] raise: Error.
	self node propertyAt: #foo put: true.
	self assert: (self node propertyAt: #foo)
]

{ #category : #'testing-properties' }
RBProgramNodeTest >> testPropertyAtIfAbsent [
	self assert: (self node propertyAt: #foo ifAbsent: [ true ]).
	self node propertyAt: #foo put: true.
	self assert: (self node propertyAt: #foo ifAbsent: [ false ])
]

{ #category : #'testing-properties' }
RBProgramNodeTest >> testPropertyAtIfAbsentPut [
	self assert: (self node propertyAt: #foo ifAbsentPut: [ true ]).
	self assert: (self node propertyAt: #foo ifAbsentPut: [ false ])
]

{ #category : #'testing-properties' }
RBProgramNodeTest >> testRemoveProperty [
	self should: [ self node removeProperty: #foo ] raise: Error.
	self node propertyAt: #foo put: true.
	self assert: (self node removeProperty: #foo)
]

{ #category : #'testing-properties' }
RBProgramNodeTest >> testRemovePropertyIfAbsent [
	self assert: (self node removeProperty: #foo ifAbsent: [ true ]).
	self node propertyAt: #foo put: true.
	self assert: (self node removeProperty: #foo ifAbsent: [ false ])
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceAssignment [
	| tree |
	tree := self parseMethod: 'run sum := 2 + 2'.
	tree body statements first replaceWith: (self parseExpression: 'multpppp := 2 * 2').
	self assert: tree newSource
		  equals: 'run
	multpppp := 2 * 2'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceBlock [
	| tree |
	tree := self parseMethod: 'run self foo ifNil: [ ^ true ]'.
	tree body statements first arguments first replaceWith: (self parseExpression: '[ ^ false ]').
	self assert: tree newSource
		  equals: 'run
	self foo ifNil: [ ^ false ]'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceLiteral [
	| tree |
	tree := self parseMethod: 'run "1" 123 "2"'.
	tree body statements first replaceWith: (self parseExpression: '$a').
	self assert: tree newSource equals: 'run "1" $a "2"'.
	tree := self parseMethod: 'run "1" 123 "2"'.
	tree body statements first replaceWith: (self parseExpression: 'zork').
	self assert: tree newSource equals: 'run "1" zork "2"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceLiteralArray1 [
	| tree |
	tree := self parseMethod: 'run "1" #(1 2 3) "2"'.
	tree body statements first replaceWith: (self parseExpression: '#[1 2 3]').
	self assert: tree newSource equals: 'run "1" #[1 2 3] "2"'.
	tree := self parseMethod: 'run "1" #(1 2 3) "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource equals: 'run "1" 123 "2"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceLiteralArray2 [
	| tree |
	tree := self parseMethod: 'run "1" #[1 2 3] "2"'.
	tree body statements first replaceWith: (self parseExpression: '#(1 2 3)').
	self assert: tree newSource equals: 'run "1" #(1 2 3) "2"'.
	tree := self parseMethod: 'run "1" #[1 2 3] "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource equals: 'run "1" 123 "2"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceMessage [
	| tree |
	tree := self parseMethod: 'run "1" self "2" run "3"'.
	tree body statements first replaceWith: (self parseExpression: 'self runCase').
	self assert: tree newSource equals: 'run "1" self "2" runCase "3"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceMessageArgument [
	| tree |
	tree := self parseMethod: 'foo "1" self "2" foo: "3" foo "4"'.
	tree body statements first arguments first replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource equals: 'foo "1" self "2" foo: "3" bar "4"'.
	tree := self parseMethod: 'foo "1" self "2" foo: "3" foo "4"'.
	tree body statements first arguments first replaceWith: (self parseExpression: 'bar msg1 msg2').
	self assert: tree newSource equals: 'foo "1" self "2" foo: "3" bar msg1 msg2 "4"'.
	tree := self parseMethod: 'foo "1" self "2" foo: "3" foo bar "4"'.
	tree body statements first arguments first replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource equals: 'foo "1" self "2" foo: "3" bar "4"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceMessageReceiver [
	| tree |
	tree := self parseMethod: 'foo "1" self "2" foo: "3" 123 "4"'.
	tree body statements first receiver replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource equals: 'foo "1" bar "2" foo: "3" 123 "4"'.
	tree := self parseMethod: 'foo "1" self "2" foo: "3" 123 "4"'.
	tree body statements first receiver replaceWith: (self parseExpression: 'bar msg1 msg2').
	self assert: tree newSource equals: 'foo "1" bar msg1 msg2 "2" foo: "3" 123 "4"'.
	tree := self parseMethod: 'foo "1" self foo "2" foo: "3" 123 "4"'.
	tree body statements first receiver replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource equals: 'foo "1" bar "2" foo: "3" 123 "4"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceMethodBinary [
	| tree |
	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #runCase andArguments: #().
	self assert: tree newSource equals: 'runCase "2" ^ "3" 4 "5"'.
	
	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #~~ andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource equals: '~~ "1" first "2" ^ "3" 4 "5"'.

	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #assert: andArguments: (Array with: (RBVariableNode named: 'first')).
	self assert: tree newSource equals: 'assert: "1" first "2" ^ "3" 4 "5"'.

	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #assert:description: andArguments: (Array with: (RBVariableNode named: 'first') with: (RBVariableNode named: 'second')).
	self assert: tree newSource equals: 'assert: first description: second "2" ^ "3" 4 "5"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceMethodKeyword [
	| tree |
	tree := self parseMethod: 'deny: "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #runCase andArguments: #().
	self assert: tree newSource equals: 'runCase "2" ^ "3" 4 "5"'.
	
	tree := self parseMethod: 'deny: "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #~~ andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource equals: '~~ "1" first "2" ^ "3" 4 "5"'.

	tree := self parseMethod: 'deny: "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #assert: andArguments: (Array with: (RBVariableNode named: 'first')).
	self assert: tree newSource equals: 'assert: "1" first "2" ^ "3" 4 "5"'.

	tree := self parseMethod: 'deny: "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #assert:description: andArguments: (Array with: (RBVariableNode named: 'first') with: (RBVariableNode named: 'second')).
	self assert: tree newSource equals: 'assert: first description: second "2" ^ "3" 4 "5"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceMethodKeywordLong [
	| tree |
	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #runCase andArguments: #().
	self assert: tree newSource equals: 'runCase "4" ^ "5" 6 "7"'.
	
	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #~~ andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource equals: '~~ first "4" ^ "5" 6 "7"'.

	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #assert: andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource equals: 'assert: first "4" ^ "5" 6 "7"'.

	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #assert:description: andArguments: (Array with: (self parseExpression: 'first') with: (self parseExpression: 'second')).
	self assert: tree newSource equals: 'assert: "1" first "2" description: "3" second "4" ^ "5" 6 "7"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceMethodUnary [
	| tree |
	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #runCase andArguments: #().
	self assert: tree newSource equals: 'runCase "1" ^ "2" 3 "4"'.
	
	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #~~ andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource equals: '~~ first "1" ^ "2" 3 "4"'.

	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #assert: andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource equals: 'assert: first "1" ^ "2" 3 "4"'.

	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #assert:description: andArguments: (Array with: (self parseExpression: 'first') with: (self parseExpression: 'second')).
	self assert: tree newSource equals: 'assert: first description: second "1" ^ "2" 3 "4"'
]

{ #category : #'testing-replacing' }
RBProgramNodeTest >> testReplaceVariable [
	| tree |
	tree := self parseMethod: 'run "1" foo "2"'.
	tree body statements first replaceWith: (self parseExpression: 'zork').
	self assert: tree newSource equals: 'run "1" zork "2"'.
	tree := self parseMethod: 'run "1" foo "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource equals: 'run "1" 123 "2"'
]

{ #category : #'testing-comments' }
RBProgramNodeTest >> testSetCommentsToNil [

	self node comments: nil.
	self assert: self node comments equals: #()
]
