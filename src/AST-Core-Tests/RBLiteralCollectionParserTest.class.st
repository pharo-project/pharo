Class {
	#name : 'RBLiteralCollectionParserTest',
	#superclass : 'RBParserTest',
	#category : 'AST-Core-Tests-Parser',
	#package : 'AST-Core-Tests',
	#tag : 'Parser'
}

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testCollectionWithDollarCharacterIsNotLiteralArrayCollection [ 

	| compiler |
	compiler := OpalCompiler new.
	self assert: (compiler evaluate: 
		'{ $: }' )
		
		equals: #($:).
	
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testDictionary [

	| compiler dict |
	compiler := OpalCompiler new.
	dict :=  (compiler evaluate: 
		'{ :Dictionary #a -> 33 . #b -> 44}' ).
	self assert: (dict at: #a)  equals: 33
	
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testDictionaryBorkenPlainText [

	self should: [ { :Dictionry #a -> 33 . #b -> 44 } ] raise: Error.
	self should: [  { :Se 1 . 2 } ] raise: Error.
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testDictionaryPlainText [

	| dict |
	dict := { :Dictionary #a -> 33 . #b -> 44}.
	self assert: (dict at: #a)  equals: 33
	
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testDynamicArrayIsWorking [

	self assert: { } equals: #().
	self assert: { 1 . 2 } equals: #(1 2).
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testLiteralSet [

	| compiler |
	compiler := OpalCompiler new.
	self assert: (compiler evaluate: 
		'{ :Set 1 . 2 . 1}' )
		
		equals: #(1 2) asSet.
	
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testNestedLiteralSet [

	| compiler |
	compiler := OpalCompiler new.
	self assert: (compiler evaluate: 
		'{ :Set 1 . {  :Set 2 . 2 } . 1}' )
		
		equals:  (Set new add: 1 ; add: (Set new add: 2; add: 2; yourself);  add: 1 ;yourself).
	
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testOrderedCollection [

	| compiler |
	compiler := OpalCompiler new.
	self 
		assert: (compiler evaluate: 
		'{ :OrderedCollection 1 . 2 . 1 . 3}' )
		equals: #(1  2 1 3) asOrderedCollection.
	
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testParseDynamicArrayAgain [

	| arrayNode |
	arrayNode := (RBParser parseExpression: '{  1 . 2 }').
	self assert: arrayNode class equals: RBArrayNode.
	self assert: arrayNode statements first value equals: 1.
	self assert: arrayNode statements second value equals: 2.
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testParseDynamicSet [

	| literalCollectionNode |
	literalCollectionNode := (RBParser parseExpression: '{ :Set  1 . 2 }').
	self assert: literalCollectionNode class  equals: RBLiteralCollectionNode.
	self assert: literalCollectionNode collectionClass name equals: 'Set'.
	self assert: literalCollectionNode statements first value equals: 1.
	self assert: literalCollectionNode statements second value equals: 2.
]

{ #category : 'tests' }
RBLiteralCollectionParserTest >> testParseDynamicSetPlainText [

	| set |
	set := { :Set  1 . 2 . 2 }.
	self assert: set size equals: 2
]
