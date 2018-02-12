Class {
	#name : #RBFormatterTest,
	#superclass : #TestCase,
	#category : #'AST-Tests-Core'
}

{ #category : #private }
RBFormatterTest >> formatClass: aClass [
	aClass selectors
		do: [ :each | self formatClass: aClass selector: each ]
]

{ #category : #private }
RBFormatterTest >> formatClass: aClass selector: aSymbol [
	| source tree1 tree2 |
	source := aClass sourceCodeAt: aSymbol.
	tree1 := RBParser parseMethod: source.
	tree2 := RBParser
		parseMethod:
			(self formatterClass new format: tree1)
		onError: [ :err :pos | self assert: false ].
	self assert: tree1 equals: tree2
]

{ #category : #private }
RBFormatterTest >> formatterClass [
  ^BISimpleFormatter
]

{ #category : #testing }
RBFormatterTest >> testCoreSystem [
	#(#Object #Behavior #Boolean #True #False #Integer #SmallInteger #Collection #String)
		do: [ :each | 
			| class |
			class := Smalltalk globals classNamed: each.
			self
				formatClass: class;
				formatClass: class class ]
]

{ #category : #testing }
RBFormatterTest >> testFormatPragmaWithLastIsSymbolArgument [
	| inputSource tree outputSource |
	"formatter should not create invalid code by removing whitespace between #= and >"
	inputSource := 'foo  <selector: #= > ^ self'.
	tree := RBParser parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [RBParser parseMethod: outputSource] raise: SyntaxErrorNotification.
	
	"already worked and still should for non-symbol arguments"
	inputSource := 'foo  <selector: 0 > ^ self'.
	tree := RBParser parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [RBParser parseMethod: outputSource] raise: SyntaxErrorNotification.
	
	"already worked and should still work for pragmas without arguments"
	inputSource := 'foo  <selector> ^ self'.
	tree := RBParser parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [RBParser parseMethod: outputSource] raise: SyntaxErrorNotification


]

{ #category : #testing }
RBFormatterTest >> testLiteralDynamicArray [
	| source tree1 |
	source := 'foo ^ { ''film'' . ''FILM''} '.
	tree1 := RBParser parseMethod: source.
	self assert: ((self formatterClass new format: tree1) occurrencesOf: $.) equals: 1
]

{ #category : #testing }
RBFormatterTest >> testPreserveLiteralArrayFormat [
	| inputSource literalArrayNode |
	"symbols within a literal array can omit the # character, if it is used that way,
	the formatter should not add a # character but just use the source form."
	inputSource := '#(#withnumbersign nonumbersign ''string'')'.
   literalArrayNode := RBParser parseExpression: inputSource.
	self assert: literalArrayNode source equals: literalArrayNode formattedCode

]

{ #category : #testing }
RBFormatterTest >> testPreserveLiteralNumberFormat [
	| inputSource numbersNode |
	"Literal numbers can be written in different ways, integer, integer with
	radix float, fractional constants. The formatter should not change the
	formatting."
	inputSource := '#(1 4r33 16r0F 0.02 2e-2 -1)'.
   numbersNode := RBParser parseExpression: inputSource.
	self assert: numbersNode source equals: numbersNode formattedCode

]
