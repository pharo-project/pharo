Class {
	#name : #RBMethodNodeTest,
	#superclass : #TestCase,
	#category : #'AST-Tests-Core'
}

{ #category : #helpers }
RBMethodNodeTest >> methodWithArg: someArgName and: someAnotherArgName [

]

{ #category : #tests }
RBMethodNodeTest >> testAddingMethodProperties [

	| ast |
	ast := RBParser parseMethod: 'one ^ self'.
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

{ #category : #tests }
RBMethodNodeTest >> testSelectorAndArgumentNames [

	self assert: (RBParser parseMethod: 'one ^ self') selectorAndArgumentNames equals: 'one'.
	self assert: (RBParser parseMethod: 'one:   aOne ^ self') selectorAndArgumentNames equals: 'one:   aOne'.
	self assert: (RBParser parseMethod: 'one:   aOne two:   aTwo ^ self') selectorAndArgumentNames equals: 'one:   aOne two:   aTwo'.
	self assert: (RBParser parseMethod: '*   aOne') selectorAndArgumentNames equals: '*   aOne'.
	self assert: (RBParser parseMethod: '**   aOne') selectorAndArgumentNames equals: '**   aOne'
]
