"
SUnit tests for RBMethodNode
"
Class {
	#name : #RBMethodNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #helpers }
RBMethodNodeTest >> methodWithArg: someArgName and: someAnotherArgName [

]

{ #category : #helpers }
RBMethodNodeTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #tests }
RBMethodNodeTest >> testAddingMethodProperties [

	| ast |
	ast := self parseMethod: 'one ^ self'.
	self assert: ast methodProperties isNil. 
	
	ast methodPropertyAt: #testKey put: #testValue.
	ast methodPropertyAt: #testKey2 put: #testValue2.
	self assert: (ast methodPropertyAt: #testKey) equals: #testValue.
	self assert: (ast methodPropertyAt: #testKey2) equals: #testValue2.
	self assert: ast methodProperties class equals: AdditionalMethodState
]

{ #category : #tests }
RBMethodNodeTest >> testCachingMethodArguments [

	| ast |
	ast := (self class >> #methodWithArg:and:) ast.
	ast propertyAt: #argumentNames put: #(wrongArg wrongArg2).
	
	ast ensureCachedArgumentNames.
	self assert: (ast methodPropertyAt: #argumentNames) equals: #(someArgName someAnotherArgName)
]

{ #category : #'testing-comments' }
RBMethodNodeTest >> testCommentedMethodHasComments [

	| tree |
	tree := self parseMethod: 'foo: abd bar: cde  
		"this is comment"
		"this is another comment"
^ abd + cde'.

	self assert: tree hasComments.
	

]

{ #category : #tests }
RBMethodNodeTest >> testNodeForOffsetAssignment [
	"test the case of a Assignment"

	| source ast foundNode |
	source := 'method Object := Class'.
	ast := self parserClass parseMethod: source.
	foundNode := ast nodeForOffset: 9.
	self assert: foundNode class identicalTo: RBVariableNode.
	foundNode := ast nodeForOffset: 14.
	self assert: foundNode class identicalTo: RBAssignmentNode.
	foundNode := ast nodeForOffset: 19.
	self assert: foundNode class identicalTo: RBVariableNode
]

{ #category : #tests }
RBMethodNodeTest >> testNodeForOffsetMessage [
	"test the case of Messages"

	| source ast foundNode |
	source := 'method Object doit: Class'.
	ast := self parserClass parseMethod: source.
	foundNode := ast nodeForOffset: 9.
	self assert: foundNode class identicalTo: RBVariableNode.
	foundNode := ast nodeForOffset: 14.
	self assert: foundNode class identicalTo: RBMessageNode.
	foundNode := ast nodeForOffset: 22.
	self assert: foundNode class identicalTo: RBVariableNode
]

{ #category : #tests }
RBMethodNodeTest >> testNodeForOffsetTempDefinition [
	"test the case of Messages"

	| source ast foundNode |
	source := 'method | temp |'.
	ast := self parserClass parseFaultyMethod: source.
	foundNode := ast nodeForOffset: 12.
	self assert: foundNode class identicalTo: RBVariableNode
]

{ #category : #tests }
RBMethodNodeTest >> testNodeForOffsetVar [
	"test the case of a variable"

	| source ast foundNode |
	source := 'method Object'.
	ast := self parserClass parseMethod: source.
	foundNode := ast nodeForOffset: 9.
	self assert: foundNode class identicalTo: RBVariableNode
]

{ #category : #tests }
RBMethodNodeTest >> testSelectorAndArgumentNames [

	self assert: (self parseMethod: 'one ^ self') selectorAndArgumentNames equals: 'one'.
	self assert: (self parseMethod: 'one:   aOne ^ self') selectorAndArgumentNames equals: 'one:   aOne'.
	self assert: (self parseMethod: 'one:   aOne two:   aTwo ^ self') selectorAndArgumentNames equals: 'one:   aOne two:   aTwo'.
	self assert: (self parseMethod: '*   aOne') selectorAndArgumentNames equals: '*   aOne'.
	self assert: (self parseMethod: '**   aOne') selectorAndArgumentNames equals: '**   aOne'
]

{ #category : #tests }
RBMethodNodeTest >> testSizeOfSignatureOfBinaryMethod [
	| tree |
	tree := self
		parseMethod:
			' + x   
^ 2 + 3'.
	self assert: tree conceptualSelectorSize equals: '+' size.
	self assert: tree conceptualArgumentSize equals: 'x'  size.
	self assert: tree conceptualSignatureSize equals: '+' size + tree conceptualArgumentSize + 1
]

{ #category : #tests }
RBMethodNodeTest >> testSizeOfSignatureOfKeywordsMethod [
	| tree |
	tree := self
		parseMethod:
			'foo: abd bar: cde  
^ abd + cde'.
	self assert: tree conceptualSelectorSize equals: 'foo:bar:' size.
	self assert: tree conceptualArgumentSize equals: 'abdcde' size.
	self
		assert: tree conceptualSignatureSize
		equals: 'abdcde' size + 'foo:bar:' size + 3.
	
]

{ #category : #tests }
RBMethodNodeTest >> testSizeOfSignatureOfUnaryMethod [
	| tree |
	tree := self
		parseMethod:
			'foo  
^ 2 + 3'.
	self assert: tree conceptualSelectorSize equals: 'foo' size.
	self assert: tree conceptualArgumentSize isZero.
	self assert: tree conceptualSignatureSize equals: 'foo' size
]

{ #category : #'testing-comments' }
RBMethodNodeTest >> testUncommentMethodDoesNotHaveComments [

	| tree |
	tree := self parseMethod: 'foo: abd bar: cde  
^ abd + cde'.

	self deny: tree hasComments.
	

]
