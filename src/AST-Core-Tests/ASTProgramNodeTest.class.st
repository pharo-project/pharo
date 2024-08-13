"
SUnit tests for ASTProgramNode
"
Class {
	#name : 'ASTProgramNodeTest',
	#superclass : 'RBParseTreeTest',
	#instVars : [
		'node'
	],
	#category : 'AST-Core-Tests-Nodes',
	#package : 'AST-Core-Tests',
	#tag : 'Nodes'
}

{ #category : 'accessing' }
ASTProgramNodeTest class >> packageNamesUnderTest [
	^ #('AST-Core')
]

{ #category : 'accessing' }
ASTProgramNodeTest >> getAllNodesFromAST [
	| methodText |
	methodText :=
	'toto
		"First comment"
		| temp varaibles |
		"Second comment"
		assignement := " Third comment " #node.
		"Fourth comment"
		message "Fifth comment", ''node''.
		"Sixth comment" cascade node; nodeux "Seventh comment"; notrois.
		^ "return" nil
		'.
	^ RBParser parseMethod: methodText
]

{ #category : 'accessing' }
ASTProgramNodeTest >> getMethodString [
	^'toto
		"First comment"
		| temp varaibles |
		"Second comment"
		assignement := " Third comment " #node.
		"Fourth comment"
		message "Fifth comment", ''node''.
		"Sixth comment" cascade node; nodeux "Seventh comment"; notrois.
		^ "return" nil
		'
]

{ #category : 'accessing' }
ASTProgramNodeTest >> node [
	^ node ifNil: [ node := ASTProgramNode new ]
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddNode [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddNodeBefore [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddNodeFirst [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddNodes [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddNodesBefore [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddNodesFirst [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddReturn [
	| tree return existingReturn lastStatement |
	tree := self parseExpression: '1. 2'.
	lastStatement := tree statements last.
	tree addReturn.
	return := tree statements last.
	self assert: return start equals: lastStatement start.
	self assert: return value equals: lastStatement.
	self assert: tree statements last equals: return.
	self assert: (self parseExpression: '1. ^ 2') equals: tree.

	tree := self parseExpression: '3. ^ 4'.
	existingReturn := tree statements last.
	tree addReturn.
	return := tree statements last.
	self assert: return identicalTo: existingReturn.
	self assert: tree statements last equals: return.
	self assert: (self parseExpression: '3. ^ 4') equals: tree
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddSelfReturn [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddTemporariesNamed [
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

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testAddTemporaryNamed [
	| tree variable |
	tree := self parseExpression: '| a | a'.
	variable := tree addTemporaryNamed: 'b'.
	self assert: variable isVariable.
	self assert: variable name equals: 'b'.
	self assert: tree temporaries last equals: variable
]

{ #category : 'tests - accessing' }
ASTProgramNodeTest >> testAllComments [
	"Testing the AST comments objects. I am the first comment to be found in this test"
	self
		assert: (ASTProgramNodeTest >> #testAllComments) ast allComments first contents
		equals: 'Testing the AST comments objects. I am the first comment to be found in this test'.
	"Next test assumes this comment it's not a string like in #testComments..."
	self
		assert: (ASTProgramNodeTest >> #testAllComments) ast allComments second isString
		equals: false
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllParents [
	"check that we get all parents of a program node"

	| me parentsOf7 |
	[ :a | a + 7 ].
	me := self class >> #testAllParents.
	parentsOf7 := (me ast allChildren select: [ :n |
		               n isLiteralNode and: [ n value = 7 ] ]) first
		              allParents.
	self assert: parentsOf7 last selector equals: #+.
	self assert: parentsOf7 first equals: me ast
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsAssignment [
	| tree  |
	tree := self parserClass parseMethod:
			'methodName

				x := 33'.

	self assert: tree allStatements size equals: 1
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsBlockWithReturn [
	| tree |
	tree := self parserClass parseMethod:
			'methodName
				^ [ ^ self ] '.
	self assert: tree allStatements size equals: 2
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsBlockWithTemps [
	| tree |
	tree := self parserClass parseMethod:
			'methodName
				^ [ | tmp | tmp := 88 ] '.
	self assert: tree allStatements size equals: 3
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsDynamicArray [
	| tree |
	tree := self parserClass parseMethod:
			'methodName

				{ 1 . self send1 . self send2} '.

	self assert: tree allStatements size equals: 4
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsReturns [
	| tree stats |
	tree := self parserClass parseMethod:
			'methodName
				^ self '.
	stats := tree allStatements.
	self assert: stats size equals: 1
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsReturnsMultiple [
	| tree stats |
	tree := self parserClass parseMethod:
			'methodName
				1 ifTrue: [ ^ self ].
				^ self '.
	stats := tree allStatements.
	self assert: stats size equals: 3
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsSimpleBlock [
	| tree stats |
	tree := self parserClass parseMethod:
			'methodName
				^ [ 8 + 4 ] '.
	stats := tree allStatements.
	self assert: stats size equals: 2
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsTemps [
	| tree stats |
	tree := self parserClass parseMethod:
			'methodName
				| tmp1 tmp2 |
				tmp1 + tmp2'.
	stats := tree allStatements.
	self assert: stats size equals: 3
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAllStatementsTwoMessages [
	| tree stats |
	tree := self parserClass parseMethod:
			'methodName

				1 send1 send2'.
	stats := tree allStatements.
	self assert: stats size equals: 1
]

{ #category : 'tests' }
ASTProgramNodeTest >> testAsDoit [

	| source ast ast2 |
	source := '|a| a + 1. a - 2. a * 3'.
	ast := self parseExpression: source.
	self assert: ast isSequence.

	ast2 := ast asDoit.
	self assert: ast2 sourceCode equals: source.
	self assert: ast2 isMethod.
	self assert: ast2 body statements first selector equals: #+.

	ast2 := ast statements second asDoit.
	self assert: ast2 sourceCode equals: 'a - 2'.
	self assert: ast2 isMethod.
	self assert: ast2 body statements first selector equals: #-
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeFirstTempsAndFirstCommentGivesVariableNode [
	| ast string start |
	string := '"First comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size + 6).
	self assert: ast isVariable
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInbetweenStatements [
	| ast string start |
	string := '"Fourth comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: 'Fourth comment'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInsideACascadeNodeOnMessageNode [
	| ast string start |
	string := '"Seventh comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: 'Seventh comment'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInsideACascadeNodeOnReceiver [
	| ast string start |
	string := '"Sixth comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: 'Sixth comment'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInsideAssignementNode [
	| ast string start |
	string := '" Third comment "'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: ' Third comment '
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInsideMessageNode [
	| ast string start |
	string := '"Fifth comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: 'Fifth comment'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInsideMethodNode [
	| ast string start |
	string := '"First comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: 'First comment'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInsideReturnNode [
	| ast string start |
	string := '"return"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: 'return'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForCommentInsideSequenceNode [
	| ast string start |
	string := '"Second comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isCommentNode.
	self assert: ast contents equals: 'Second comment'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeForStatementInsideMethodNodeReturnsStatement [

	| bestNode string |

	string := self getMethodString findString: '.'.
	bestNode := (RBParser parseMethod: self getMethodString)
		        bestNodeForPosition: string + 1.
	self
		assert: bestNode isAssignment;
		assert: bestNode sourceCode
		equals: 'assignement := " Third comment " #node'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeInsideAssignementGivesSelectedValue [
	| ast string start |
	string := '#node'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isLiteralNode.
	self assert: ast value equals: #node
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeInsideAssignementGivesSelectedVariable [
	| ast string start |
	string := 'assignement'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (start to: start + string size - 1).
	self assert: ast isVariable.
	self assert: ast name equals: 'assignement'
]

{ #category : 'tests - bestNodeFor' }
ASTProgramNodeTest >> testBestNodeWithMethodSelectorGivesCommentNode [
	| ast string start |
	string := '"First comment"'.
	start := self getMethodString findString: string.
	ast := (RBParser parseMethod: self getMethodString) bestNodeFor: (1 to: start + string size - 1).
	self assert: ast isCommentNode
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorAfterArgumentReturnsVariableNode [

	| sourceCode ast bestNode |
	sourceCode := 'toto: m1
	               m1 aMessage'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 9.

	self assert: bestNode isVariable
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorAfterColonReturnsPreviousStatement [
	"toto: arg
		arg m1.|
		arg m2"
	| sourceCode ast bestNode |
	sourceCode := 'toto
	var m1.
	var m2'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 13.

	self assert: bestNode isMessage.
	self assert: bestNode selector equals: #m1
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorAfterMessageAndBeforeNewlineReturnsMessageNode [
	"toto: arg
	   arg m1 |
	m2"
	| sourceCode ast bestNode |
	sourceCode := 'toto: arg
	               arg m1
	 m2'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 33.

	self assert: bestNode isMessage.
	self assert: bestNode selector equals: #m1
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorAfterMessageAndSpaceReturnsMessageNode [
	"toto: arg
	   arg m1 | m2"
	| sourceCode ast bestNode |
	sourceCode := 'toto: arg
	               arg m1  m2'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 33.

	self assert: bestNode isMessage.
	self assert: bestNode selector equals: #m1
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorAfterMessageReturnsMessageNode [
	"toto: arg
	   arg m1| m2"
	| sourceCode ast bestNode |
	sourceCode := 'toto: arg
	               arg m1 m2'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 32.

	self assert: bestNode isMessage.
	self assert: bestNode selector equals: #m1
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorAfterVariableReturnsVariableNode [
	"toto: m1
	   m1 |aMessage"
	| sourceCode ast bestNode |
	sourceCode := 'toto: m1
	               m1 aMessage'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 28.

	self assert: bestNode isVariable
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorBeforeArgumentNextToSelectorReturnsMethodNode [

	| sourceCode ast bestNode |
	sourceCode := 'toto:m1
	               m1 aMessage'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 6.

	self assert: bestNode isMethod
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorBeforeArgumentReturnsVariableNode [

	| sourceCode ast bestNode |
	sourceCode := 'toto: m1
	               m1 aMessage'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 7.

	self assert: bestNode isVariable
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorBeforeMessageAndAfterNewlineReturnsMessageNode [
	"toto: arg
	   arg m1
|	m2"
	| sourceCode ast bestNode |
	sourceCode := 'toto: arg
	               arg m1
	 m2'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 34.

	self assert: bestNode isMessage.
	self assert: bestNode selector equals: #m2
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorBeforeVariableReturnsVariableNode [

	| sourceCode ast bestNode |
	sourceCode := 'toto: m1
	               m1 aMessage'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 26.

	self assert: bestNode isVariable
]

{ #category : 'tests - adding' }
ASTProgramNodeTest >> testCursorInMiddleOfVariableReturnsVariableNode [

	| sourceCode ast bestNode |
	sourceCode := 'toto: m1
	               m1 aMessage'.

	ast := RBParser parseMethod: sourceCode.
	bestNode := ast bestNodeForPosition: 27.

	self assert: bestNode isVariable
]

{ #category : 'tests - properties' }
ASTProgramNodeTest >> testHasProperty [
	self deny: (self node hasProperty: #foo).
	self node propertyAt: #foo put: 123.
	self assert: (self node hasProperty: #foo)
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenBlockWithoutReturnExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							[ 2 ]'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenMessageWithReturnBlockExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							instVar at: #foo ifAbsentPut: [ ^ 2 ]'.
	statement := ast body statements last.
	self deny: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenMessageWithoutReturnBlockExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							instVar at: #foo ifAbsentPut: [ 2 ]'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnNodeExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ 2'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnNodeWithAssignmentExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ a := 3'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnNodeWithMessageSendExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ self foo: 2'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnWithBlockAndSequenceWithNestedBlockWithoutReturnExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ [ self foo: [ :a | a + 1 ]. ^ 2 ]'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnWithBlockWithoutReturnExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ [ 2 ]'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnWithEmptyBlockExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ [ ]'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnWithMessageWithReturnBlockExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ instVar at: #foo ifAbsentPut: [ ^ 2 ]'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnWithMessageWithoutReturnBlockExpecTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ instVar at: #foo ifAbsentPut: [ 2 ]'.
	statement := ast body statements last.
	self assert: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenReturnWithWhileFalseBlockWithReturnExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							[ ^ false ] whileFalse: [ true ]'.
	statement := ast body statements last.
	self deny: statement hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenSequenceWithLastReturnAndNonLocalReturnExpectTrue [

	| ast sequence |
	ast := RBParser parseMethod: 'a1
							self foo.
							instVar := true ifFalse: [ ^ 1 ].
							instVar isOdd ifTrue: [ instVar := instVar + 1 ].
							^ self end'.
	sequence := RBSequenceNode statements: ast body statements allButFirst.
	sequence parent: ast body.
	self assert: sequence hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenSequenceWithLastReturnExpectTrue [

	| ast sequence |
	ast := RBParser parseMethod: 'a1
							self foo.
							instVar := true ifFalse: [ 1 ].
							^ 2'.
	sequence := RBSequenceNode statements: ast body statements allButFirst.
	sequence parent: ast body.
	self assert: sequence hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenSequenceWithoutAnyReturnsExpectTrue [

	| ast sequence |
	ast := RBParser parseMethod: 'a1
							self foo.
							instVar := true ifFalse: [ 1 ]
							self end'.
	sequence := RBSequenceNode statements: ast body statements allButFirst.
	sequence parent: ast body.
	self assert: sequence hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenSequenceWithoutLastReturnAndMultipleNonLocalReturnExpectFalse [

	| ast sequence |
	ast := RBParser parseMethod: 'a1
							self foo.
							[  instVar isOdd ifTrue: [ ^ 2 ]. 
								instVar := instVar + 1] 
								whileTrue: [ instVar < 10 ].
							instVar := true ifFalse: [ ^ 1 ].
							self end'.
	sequence := RBSequenceNode statements: ast body statements allButLast.
	sequence parent: ast body.
	self deny: sequence hasSameExitPoint 
]

{ #category : 'tests - hasSameExitPoint' }
ASTProgramNodeTest >> testHasSameExitPointWhenSequenceWithoutLastReturnAndNonLocalReturnExpectFalse [

	| ast sequence |
	ast := RBParser parseMethod: 'a1
							self foo.
							instVar := true ifFalse: [ ^ 1 ]
							self end'.
	sequence := RBSequenceNode statements: ast body statements allButFirst.
	sequence parent: ast body.
	self deny: sequence hasSameExitPoint 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenFirstMessageInACascadeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo; bar; end'.
	statement := ast body statements last messages first.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenItIsReceiverExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo bar'.
	statement := ast body statements last receiver.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenLastMessageInACascadeAndCascadeIsEssentialExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               a := self foo; bar; end'.
	statement := ast body statements last value messages last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenLastMessageInACascadeAndCascadeIsNotEssentialExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo; bar; end'.
	statement := ast body statements last value messages last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenMessageInACascadeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo; bar; end'.
	statement := ast body statements last messages second.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenNodeIsArgumentExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self ars: self foo'.
	statement := ast body statements first arguments first.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenOnlyNodeInABlockNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               [self foo]'.
	statement := ast body statements last body statements first.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenPartOfABlockNodeExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               [self foo]'.
	statement := ast body statements last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenPartOfAReturnStatementExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               ^ self foo'.
	statement := ast body statements last value.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenPartOfASequenceExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. self bar. ^ 1'.
	statement := ast body statements first.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenPartOfASequenceLastNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. self bar'.
	statement := ast body statements last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenPartOfASequenceOneStatementOnlyExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo'.
	statement := ast body statements last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenPartOfAnAssignementInBlockNodeExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               [ a := self foo]'.
	statement := ast body statements last body statements first value.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenTwoMessagesInACascadeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               a := self foo; bar'.
	statement := ast body statements last value messages last.
	self deny: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenValueOfAnAssignmentExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               |m1| m1 := self foo'.
	statement := ast body statements last value.
	self assert: statement isEssential 
]

{ #category : 'tests - isEssential' }
ASTProgramNodeTest >> testIsEssentialWhenVariableOfAnAssignmentExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               |m1| m1 := self foo'.
	statement := ast body statements last variable.
	self assert: statement isEssential 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenBlockNodeInSequenceNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. [self bar]. ^ self end'.
	statement := ast body statements second.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenFirstNodeInSequenceNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. self bar. ^ self end'.
	statement := ast body statements first.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenInSequenceNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. self bar. ^ self end'.
	statement := ast body statements second.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenLastNodeInSequenceExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. self end'.
	statement := ast body statements last.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenLastNodeIsReturnStatementInSequenceExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. ^ self end'.
	statement := ast body statements last.
	self assert: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsArrayExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               { self foo . a := self end }'.
	statement := ast body statements last.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsAssignmentExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. a := self end'.
	statement := ast body statements last.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsAssignmentInSequenceExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. a := self end. ^ 1'.
	statement := ast body statements second.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsAssignmentValueExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. a := self end'.
	statement := ast body statements last value.
	self assert: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsAssignmentVariableExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self foo. a := self end'.
	statement := ast body statements last variable.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsLastInBlockExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               [ self foo. self bar ]'.
	statement := ast body statements first body statements second.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsNotLastInBlockExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               [ self foo. self bar ]'.
	statement := ast body statements first body statements first.
	self deny: statement isUsedAsReturnValue 
]

{ #category : 'tests - isUsedAsReturnValue' }
ASTProgramNodeTest >> testIsUsedAsReturnValueWhenNodeIsPartOfArrayExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               { self foo . a := self end }'.
	statement := ast body statements last statements first.
	self assert: statement isUsedAsReturnValue 
]

{ #category : 'tests' }
ASTProgramNodeTest >> testNumberOfSentMessages [
	| tree messages |
	tree := self parserClass parseMethod:
			'methodName
				| temp |
				1 send1 send2; send3.
				temp := [:each | {4 send4} send5].
				temp send6.
				0 ifTrue: [ 1 ]'.
	messages := tree sentMessages.
	self assert: messages size equals: 7.
	"we count ifTrue:"
	1 to: 6 do:
		[ :i | self assert: (messages includes: ('send' , i printString) asSymbol) ]
]

{ #category : 'tests - properties' }
ASTProgramNodeTest >> testPropertyAt [
	self should: [ self node propertyAt: #foo ] raise: Error.
	self node propertyAt: #foo put: true.
	self assert: (self node propertyAt: #foo)
]

{ #category : 'tests - properties' }
ASTProgramNodeTest >> testPropertyAtIfAbsent [
	self assert: (self node propertyAt: #foo ifAbsent: [ true ]).
	self node propertyAt: #foo put: true.
	self assert: (self node propertyAt: #foo ifAbsent: [ false ])
]

{ #category : 'tests - properties' }
ASTProgramNodeTest >> testPropertyAtIfAbsentPut [
	self assert: (self node propertyAt: #foo ifAbsentPut: [ true ]).
	self assert: (self node propertyAt: #foo ifAbsentPut: [ false ])
]

{ #category : 'tests' }
ASTProgramNodeTest >> testPropertyAtIfPresent [
	self assert: (self node propertyAt: #foo ifPresent:[ :p | #bad ]) equals: nil.
	self node propertyAt: #foo put: true.
	self assert: (self node propertyAt: #foo ifPresent:[ :p | p ]).
]

{ #category : 'tests - properties' }
ASTProgramNodeTest >> testPropertyAtIfPresentIfAbsent [
	self assert: (self node propertyAt: #foo ifPresent:[ false ] ifAbsent: [ true ]) equals: true.
	self node propertyAt: #foo put: true.
	self assert: (self node propertyAt: #foo ifPresent:[ true ] ifAbsent: [ false ]).
	self assert: (self node propertyAt: #toto ifPresent:[ false ] ifAbsent: [ true ])
]

{ #category : 'tests - properties' }
ASTProgramNodeTest >> testRemoveProperty [
	self should: [ self node removeProperty: #foo ] raise: Error.
	self node propertyAt: #foo put: true.
	self assert: (self node removeProperty: #foo)
]

{ #category : 'tests - properties' }
ASTProgramNodeTest >> testRemovePropertyIfAbsent [
	self assert: (self node removeProperty: #foo ifAbsent: [ true ]).
	self node propertyAt: #foo put: true.
	self assert: (self node removeProperty: #foo ifAbsent: [ false ])
]

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceAssignment [
	| tree expectedTree |
	tree := self parseMethod: 'run sum := 2 + 2'.
	expectedTree := self parseMethod: 'run multpppp := 2 * 2'.

	tree body statements first replaceWith: (self parseExpression: 'multpppp := 2 * 2').
	self assert: tree equals: expectedTree
]

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceBlock [
	| tree expectedTree |
	tree := self parseMethod: 'run self foo ifNil: [ ^ true ]'.
	expectedTree := self parseMethod: 'run self foo ifNil: [ ^ false ]'.

	tree body statements first arguments first replaceWith: (self parseExpression: '[ ^ false ]').
	self assert: tree  equals: expectedTree
]

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceLiteral [
	| tree |
	tree := self parseMethod: 'run "1" 123 "2"'.
	tree body statements first replaceWith: (self parseExpression: '$a').
	self assert: tree newSource equals: 'run "1" $a "2"'.
	tree := self parseMethod: 'run "1" 123 "2"'.
	tree body statements first replaceWith: (self parseExpression: 'zork').
	self assert: tree newSource equals: 'run "1" zork "2"'
]

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceLiteralArray1 [
	| tree |
	tree := self parseMethod: 'run "1" #(1 2 3) "2"'.
	tree body statements first replaceWith: (self parseExpression: '#[1 2 3]').
	self assert: tree newSource equals: 'run "1" #[ 1 2 3 ] "2"'.
	tree := self parseMethod: 'run "1" #(1 2 3) "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource equals: 'run "1" 123 "2"'
]

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceLiteralArray2 [
	| tree |
	tree := self parseMethod: 'run "1" #[1 2 3] "2"'.
	tree body statements first replaceWith: (self parseExpression: '#(1 2 3)').
	self assert: tree newSource equals: 'run "1" #( 1 2 3 ) "2"'.
	tree := self parseMethod: 'run "1" #[1 2 3] "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource equals: 'run "1" 123 "2"'
]

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceMessage [
	| tree |
	tree := self parseMethod: 'run "1" self "2" run "3"'.
	tree body statements first replaceWith: (self parseExpression: 'self runCase').
	self assert: tree newSource equals: 'run "1" self "2" runCase "3"'
]

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceMessageArgument [
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

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceMessageReceiver [
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

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceMethodBinary [
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

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceMethodKeyword [
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

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceMethodKeywordLong [
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

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceMethodUnary [
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

{ #category : 'tests - replacing' }
ASTProgramNodeTest >> testReplaceVariable [
	| tree |
	tree := self parseMethod: 'run "1" foo "2"'.
	tree body statements first replaceWith: (self parseExpression: 'zork').
	self assert: tree newSource equals: 'run "1" zork "2"'.
	tree := self parseMethod: 'run "1" foo "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource equals: 'run "1" 123 "2"'
]

{ #category : 'tests - messages' }
ASTProgramNodeTest >> testSentMessages [
	| tree messages |
	tree := self
		parseRewriteMethod:
			'methodName
				| temp |
				1 send1 send2; send3.
				temp := [:each | {4 send4} send5].
				temp send6 `{:node | node notASentMessage}'.
	messages := tree sentMessages.
	self assert: messages size equals: 6.
	1 to: 6 do:
		[ :i | self assert: (messages includes: ('send' , i printString) asSymbol) ]
]

{ #category : 'tests - comments' }
ASTProgramNodeTest >> testSetCommentsToNil [

	self node comments: nil.
	self assert: self node comments equals: #()
]

{ #category : 'tests' }
ASTProgramNodeTest >> testWithAllParents [
	"check that we get all parents of a program node, and me"

	| me parentsOf7 |
	[ :a | a + 7 ].
	me := self class >> #testAllParents.
	parentsOf7 := (me ast allChildren select: [ :n |
		               n isLiteralNode and: [ n value = 7 ] ]) first
		              withAllParents.
	self assert: parentsOf7 last value equals: 7.
	self assert: parentsOf7 first equals: me ast
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenArrayNodeWithFirstReturnExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							{ ^ 1 . #a }'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenArrayNodeWithLastReturnExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							{ ^ 1 }'.
	statement := ast body statements last.
	self assert: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenBlockNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							[ 2 ]'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenEmptyArrayNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							{ }'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenEmptySequenceExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1'.
	statement := ast body.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenMessageNodeIsInlinedIfAndHasOneArgumentExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							true ifTrue: [ ]'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenMessageNodeIsInlinedIfAndHasTwoArgumentExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							true ifTrue: [ ] ifFalse: [ ]'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenMessageNodeIsInlinedIfAndHasTwoArgumentsAndBothArgsHaveReturnExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							true ifTrue: [ ^ 1 ] ifFalse: [ ^ 1 ]'.
	statement := ast body statements last.
	self assert: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenMessageNodeIsInlinedIfAndHasTwoArgumentsAndFirstArgHasReturnExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							true ifTrue: [ ^ 1 ] ifFalse: [ ]'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenMessageNodeIsInlinedIfAndHasTwoArgumentsAndSecondArgHasReturnExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							true ifTrue: [ ] ifFalse: [ ^ 1 ]'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenMessageNodeIsNotInlinedIfExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							self foo'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenMethodNodeWithLastReturnExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ 1'.
	statement := ast.
	self assert: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenNodeIsNotReturnExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self bar'.
	statement := ast body statements last.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenNodeIsReturnExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               ^ self bar'.
	statement := ast body statements last.
	self assert: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenNodeIsSequenceWithLastReturnNodeExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               ^ self bar'.
	statement := ast body.
	self assert: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenNodeIsSequenceWithoutLastReturnNodeExpectFalse [

	| ast statement |
	ast := RBParser parseMethod: 'a1
	               self bar'.
	statement := ast body.
	self deny: statement lastIsReturn 
]

{ #category : 'tests - lastIsReturn' }
ASTProgramNodeTest >> testlastIsReturnWhenReturnNodeWithBlockNodeExpectTrue [

	| ast statement |
	ast := RBParser parseMethod: 'a1
							^ [ 2 ]'.
	statement := ast body statements last.
	self assert: statement lastIsReturn 
]
