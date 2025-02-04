"
SUnit tests for BISimpleFormatter
"
Class {
	#name : 'OCFormatterTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Formatter',
	#package : 'AST-Core-Tests',
	#tag : 'Formatter'
}

{ #category : 'private' }
OCFormatterTest >> formatClass: aClass [
	aClass selectors
		do: [ :each | self formatClass: aClass selector: each ]
]

{ #category : 'private' }
OCFormatterTest >> formatClass: aClass selector: aSymbol [
	| source tree1 tree2 |
	source := aClass sourceCodeAt: aSymbol.
	tree1 := self parserClass parseMethod: source.
	tree2 := self parserClass
		parseMethod: (self formatterClass new format: tree1)
		onError: [ :err :pos | self assert: false ].
	self assert: tree1 equals: tree2
]

{ #category : 'private' }
OCFormatterTest >> formatterClass [
  ^OCSimpleFormatter
]

{ #category : 'tests' }
OCFormatterTest >> testCoreSystem [
	#(#Object #Behavior #Boolean #True #False #Integer #SmallInteger #Collection #String)
		do: [ :each |
			| class |
			class := testingEnvironment classNamed: each.
			self
				formatClass: class;
				formatClass: class class ]
]

{ #category : 'tests' }
OCFormatterTest >> testFormatPragmaWithLastIsSymbolArgument [
	| inputSource tree outputSource |
	"formatter should not create invalid code by removing whitespace between #= and >"
	inputSource := 'foo  <selector: #= > ^ self'.
	tree := self parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [self parseMethod: outputSource] raise: OCCodeError.

	"already worked and still should for non-symbol arguments"
	inputSource := 'foo  <selector: 0 > ^ self'.
	tree := self parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [self parseMethod: outputSource] raise: OCCodeError.

	"already worked and should still work for pragmas without arguments"
	inputSource := 'foo  <selector> ^ self'.
	tree := self parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [self parseMethod: outputSource] raise: OCCodeError
]

{ #category : 'tests' }
OCFormatterTest >> testLiteralDynamicArray [
	| source tree1 |
	source := 'foo ^ { ''film'' . ''FILM''} '.
	tree1 := self parseMethod: source.
	self assert: ((self formatterClass new format: tree1) occurrencesOf: $.) equals: 1
]

{ #category : 'tests' }
OCFormatterTest >> testParseError [

	| source node |
	source := ' +  foo:  )'.
	node := self parseFaultyExpression: source.
	self assert: node formattedCode equals: source
]

{ #category : 'tests' }
OCFormatterTest >> testPreserveLiteralArrayFormat [
	| inputSource literalArrayNode |
	"symbols within a literal array can omit the # character, if it is used that way,
	the formatter should not add a # character but just use the source form."
	inputSource := '#( #withnumbersign nonumbersign ''string'' )'.
   literalArrayNode := self parseExpression: inputSource.
	self assert: literalArrayNode source equals: literalArrayNode formattedCode
]

{ #category : 'tests' }
OCFormatterTest >> testPreserveLiteralNumberFormat [
	| inputSource numbersNode |
	"Literal numbers can be written in different ways, integer, integer with
	radix float, fractional constants. The formatter should not change the
	formatting."
	inputSource := '#( 1 4r33 16r0F 0.02 2e-2 -1 )'.
   numbersNode := self parseExpression: inputSource.
	self assert: numbersNode source equals: numbersNode formattedCode
]
