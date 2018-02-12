"
SUnit tests for RBParser
"
Class {
	#name : #RBParserTest,
	#superclass : #TestCase,
	#category : #'AST-Tests-Core'
}

{ #category : #accessing }
RBParserTest class >> parserClass [
	^ RBParser
]

{ #category : #accessing }
RBParserTest >> compare: anObject to: anotherObject [ 
	self assert: anObject hash equals: anotherObject hash.
	self assert: anObject equals: anotherObject
]

{ #category : #accessing }
RBParserTest >> exampleClasses [
	^ Array with: RBParser with: RBScanner with: RBProgramNode with: BIConfigurableFormatter
]

{ #category : #private }
RBParserTest >> parseError: each [
	RBParser 
		parseExpression: each first 
		onError: [ :string :pos | ^ self assert: pos equals: each last ].
	self assert: false description: 'Parser didn''t fail'
]

{ #category : #private }
RBParserTest >> parseFaultyExpression: text [
	^ self parserClass parseFaultyExpression: text
]

{ #category : #private }
RBParserTest >> parseFaultyMethod: text [
	^RBParser parseFaultyMethod: text.
]

{ #category : #accessing }
RBParserTest >> parserClass [
	^ self class parserClass
]

{ #category : #'tests parsing' }
RBParserTest >> testArray [
	| tree |
	#(('{}' 0 0) ('{.}' 0 1) ('{..}' 0 2) ('{foo. bar}' 2 1) ('{foo. bar.}' 2 2) ('{foo. bar. .}' 2 3) ('{. foo. bar}' 2 2) ('{foo.. bar}' 2 2))
	 do: [ :each |
		tree := self parserClass parseExpression: each first.
		self assert: tree statements size equals: each second.
		self assert: tree periods size equals: each last.
		self assert: tree left equals: 1.
		self assert: tree right equals: each first size ]
]

{ #category : #tests }
RBParserTest >> testArrayNodesArentArrays [
"Although an RBArrayNode represents an array, it is not an Array-like object.
Therefore it shouldn't return true for message #isArray (like an instance of a real
array would"
	| arrayNode nonArrayNode |
	arrayNode := self parserClass parseExpression: '{ 1 . 2 . 3}'.
				
	nonArrayNode := self parserClass parseExpression: 'a := 3'.
				
	self deny: arrayNode isArray.
	self assert: arrayNode isDynamicArray.
	self deny: nonArrayNode isDynamicArray

]

{ #category : #'tests interval' }
RBParserTest >> testBestNodeFor [
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo: 
			[:each | 
			each sourceInterval isEmpty 
				ifFalse: 
					[self assert: ((tree bestNodeFor: each sourceInterval) = each 
								or: [each parent isCascade and: [each parent messages last = each]])]]
]

{ #category : #'tests parsing' }
RBParserTest >> testBinarySelectors [
	"ANSI Smalltalk Standard Draft, page 32:
		binaryCharacter ::=
			'!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'
		binarySelector ::= binaryCharacter+"

	| binaryCharacters selectorStrings |

	binaryCharacters := '!%&*+,/<=>?@\~-'.
	selectorStrings := OrderedCollection new.
	binaryCharacters do: [:first |
		selectorStrings addLast: (String with: first).
		binaryCharacters do: [:second |
			selectorStrings addLast: (String with: first with: second).
			binaryCharacters do: [:third |
				selectorStrings addLast: (String with: first with: second with: third)]]].

	selectorStrings do: [:each | | methodNode messageNode |
		methodNode := RBParser parseMethod: each, 'anObject
	^self',each,'anObject'.
		self assert: methodNode selector equals: each asSymbol.
		self assert: methodNode argumentNames asArray equals: #(anObject).
		self assert: methodNode statements size equals: 1.
	
		messageNode := methodNode statements first value.
		self assert: messageNode isMessage.
		self assert: messageNode receiver name equals: #self.
		self assert: messageNode selector equals: each asSymbol.
		self assert: messageNode arguments size equals: 1.
		self assert: messageNode arguments first isValue.
		self assert: messageNode arguments first name equals: #anObject].
]

{ #category : #tests }
RBParserTest >> testBlockNodeIsUsed [
	| tree |
	" block node is not a used-node"
	tree := self parserClass parseMethod:
			'tmp [ 3+4 ]'.
	self deny: tree body statements last isUsed.
	" even a block node with non-local return is not *used* "
	tree := self parserClass parseMethod:
	      'tmp [^ 3+4 ]'.
	self deny: tree body statements last isUsed.
	
	"an explicit return node is *used*"
	tree := self parserClass parseMethod:
			'tmp ^[ 3+4 ]'.
	self assert: tree body statements last isUsed
	
]

{ #category : #tests }
RBParserTest >> testBlockReturnNode [
	| tree |
	" no explicit return"
	tree := self parserClass parseMethod:
			'tmp [ 3+4 ]'.
	self deny: tree lastIsReturn.
	" no explicit return the block local return should not count as method return "
	tree := self parserClass parseMethod:
	      'tmp [^ 3+4 ]'.
	self deny: tree lastIsReturn.
	
	" explicit return"
	tree := self parserClass parseMethod:
			'tmp ^ [ 3+4 ]'.
	self assert: tree lastIsReturn
	
]

{ #category : #'tests parsing' }
RBParserTest >> testCascade [
	self assert: (self parserClass parseExpression: ' self msg; yourself') isCascade.
	self assert: (self parserClass parseExpression: ' self msg:(arg msg:arg); yourself') isCascade.
	self should: [ self parserClass parseExpression: ' (self msg); yourself' ] raise: SyntaxErrorNotification.
	self should: [ self parserClass parseExpression: ' self ; yourself' ] raise: SyntaxErrorNotification.
	self should: [ self parserClass parseExpression: ' (self) ; yourself' ] raise: SyntaxErrorNotification
]

{ #category : #'tests replacements' }
RBParserTest >> testCascadeReplacement [
	| cascade |
	cascade := self parserClass parseExpression: 'self foo; bar; baz'.
	(cascade messages at: 2) 
		replaceWith: (self parserClass parseExpression: 'self bar: 2').
	self compare: cascade
		to: (self parserClass parseExpression: 'self foo; bar: 2; baz')
]

{ #category : #'tests comparing' }
RBParserTest >> testComparingTrees [
	self compare: self treeWithEverything to: self treeWithEverything.
	self compare: self treeWithReallyEverything to: self treeWithReallyEverything.
	
	self exampleClasses do: [ :class |
		class selectors do: [ :selector | 
			self
				compare: (class parseTreeFor: selector) 
				to: (class parseTreeFor: selector) ] ]
]

{ #category : #tests }
RBParserTest >> testCopy [
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: tree copy.
	tree := self treeWithReallyEverything.
	self compare: tree to: tree copy.
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self compare: tree to: tree copy ] ]
]

{ #category : #tests }
RBParserTest >> testCopyInContext [
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self compare: tree to: (tree copyInContext: Dictionary new) ] ]
]

{ #category : #tests }
RBParserTest >> testCreationProtocol [
	| messageNode |
	self compare: (RBMessageNode 
				receiver: (RBVariableNode named: 'self')
				selector: #+
				arguments: (Array with: (RBLiteralNode value: 0)))
		to: (self parserClass parseExpression: 'self + 0').
	messageNode := RBMessageNode receiver: (RBVariableNode named: 'self') selector: #foo.
	self compare: (RBMethodNode selector: #bar
				body: (RBSequenceNode statements: (OrderedCollection 
								with: (RBCascadeNode messages: (OrderedCollection with: messageNode with: messageNode)))))
		to: (self parserClass parseMethod: 'bar self foo; foo')
]

{ #category : #'tests comparing' }
RBParserTest >> testEqualToWithMapping [
	| tree |
	tree := self treeWithEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self assert: (tree equalTo: tree withMapping: Dictionary new) ] ]
]

{ #category : #'tests comparing' }
RBParserTest >> testEquivalentExceptRenaming [
	#(('a 3-4' 'a 4-3' false ) ('a #[3 4]' 'a #(3 4)' false ) ('a variable1 ~~ "comment" variable2' 'a variable1 ~~ variable2' true ) ('a variable1' 'a variable2' false ) ('a [:a :b | a + b]' 'a [:b :a | a + b]' false ) ('a | a b | a + b' 'a | b a | a + b' true ) ('a | a | a msg1; msg2' 'a | b | b msg2; msg2' false ) ('a c' 'a d' true ) ('a | a b | a := b. ^b msg1' 'a | a b | b := a. ^a msg1' true ) ('a | a b | a := b. ^b msg1: a' 'a | a b | b := a. ^b msg1: a' false ) ('a: b b + 4' 'a: e e + 4' true ) ('a: b b + 4' 'b: b b + 4' false ) ('a: b b: c b + c' 'a: c b: b c + b' true ) ('a: a b: b a + b' 'a: b b: a a + b' false ) ) do: [:each | self assert: ((self parserClass parseMethod: each first)
				equalTo: (self parserClass parseMethod: (each at: 2))
				exceptForVariables: #('c' ))
				== each last ]
]

{ #category : #tests }
RBParserTest >> testFormatter [
	self exampleClasses do: [ :class |
		class selectors do: [ :selector | 
		self 
			compare: (class parseTreeFor: selector)
			to: (self parserClass parseMethod: (class parseTreeFor: selector) printString) ] ]
]

{ #category : #'tests interval' }
RBParserTest >> testIntervals [
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo: 
			[:each | 
			(each parent isNil or: [each parent isCascade not and: [ each parent isLiteralNode not]]) 
				ifTrue: 
					[| newNode source |
					source := tree source copyFrom: each start to: each stop.
					each isPragma ifFalse: [
						newNode := each isMethod 
								ifTrue: [self parserClass parseMethod: source]
								ifFalse: [self parserClass parseExpression: source].
						self compare: each to: newNode]]]
]

{ #category : #'tests parsing' }
RBParserTest >> testInvalidPragmaPosition [
	#(
	'foo self. <foo: $a>'
	'<foo: $a>'
	'foo [:x | <foo: $a> ] value'
	'foo ^ <foo: $a>'
	) 
	do: [ :each | self should: [ self parserClass parseMethod: each ] raise: SyntaxErrorNotification ]
]

{ #category : #tests }
RBParserTest >> testIsA [
	| nodes types |
	nodes := Bag new.
	types := Set new.
	#(#(#isAssignment 1) #(#isBlock 1) #(#isCascade 1) #(#isLiteralNode 2) #(#isMessage 3) #(#isMethod 1) #(#isReturn 1) #(#isSequence 2) #(#isValue 15) #(#isVariable 7) #(#isUsed 10) #(#isDirectlyUsed 9) #(#hasParentheses 1) #(#isBinary 0) #(#isPrimitive 0) #(#isImmediateNode 10) #(#isWrite 1) #(#isRead 3)) 
		do: 
			[:each | 
			each last timesRepeat: [nodes add: each first].
			types add: each first].
	self treeWithEverything nodesDo: 
			[:each | 
			types do: 
					[:sel | 
					((each respondsTo: sel) and: [each perform: sel]) 
						ifTrue: [nodes remove: sel]]].
	self assert: nodes isEmpty
]

{ #category : #'tests parsing' }
RBParserTest >> testLiteralArray [
	| tree collection |
	tree := self parserClass parseExpression: '#( a #b #''c'' . + - 1 -2 3.4 #true true #false false #nil nil "comment" ''string'' #[ 1 2 3 ] #(1 2 3))'.
	collection := OrderedCollection new.
	collection add: #a; add: #b; add: #c; add: #'.'; add: #+; add: #-; add: 1; add: -2; add: 3.4; add: #true; add: true; add: #false; add: false; add: #nil; add: nil; add: 'string'; add: #[1 2 3]; add: #(1 2 3).	
	tree value with: collection do: [ :token :value | 
		self assert: token value equals: value ]
]

{ #category : #'tests interval' }
RBParserTest >> testLiteralIntevals [
	| tree |
	tree := self parserClass parseExpression: '#(#a b #( c ))'.
	self assert: tree contents first start equals: 3.
	self assert: tree contents first stop equals: 4.
	self assert: tree contents last contents first start equals: 11
]

{ #category : #'tests matching' }
RBParserTest >> testMatchInContext [
	| tree |
	tree := self treeWithEverything.
	self assert: (tree match: tree inContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree match: tree inContext: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self assert: (tree match: tree inContext: Dictionary new) ] ]
]

{ #category : #tests }
RBParserTest >> testMethodPatterns [
	#(#('+ a ^self + a' #+) #('foo ^self foo' #foo) #('foo: a bar: b ^a + b' #foo:bar:)) 
		do: [:each | self assert: (self parserClass  parseMethodPattern: each first) == each last]
]

{ #category : #tests }
RBParserTest >> testModifying [
	| tree |
	tree := self parserClass  
				parseMethod: 'foo: a bar: b | c | self first. self second. a + b + c'.
	self deny: tree lastIsReturn.
	self deny: (tree body statements at: 2) isUsed.
	self assert: tree body statements last arguments first isUsed.
	self assert: (tree isLast: tree body statements last).
	self deny: (tree isLast: tree body statements first).
	self assert: (tree defines: 'a').
	self deny: (tree defines: 'c').
	self assert: (tree body defines: 'c').
	self deny: (tree body defines: 'a').
	tree
		addReturn;
		selector: #bar:foo:.
	(tree body)
		addTemporaryNamed: 'd';
		removeTemporaryNamed: 'c'.
	self compare: tree
		to: (self parserClass  
				parseMethod: 'bar: a foo: b | d | self first. self second. ^a + b + c').
	self 
		assert: ((tree argumentNames asSet)
				removeAll: #('a' 'b');
				yourself) isEmpty.
	self 
		assert: ((tree allDefinedVariables asSet)
				removeAll: #('a' 'b' 'd');
				yourself) isEmpty.
	tree := self parserClass  parseExpression: 'self foo: 0'.
	tree selector: #+.
	self compare: tree to: (self parserClass  parseExpression: 'self + 0').
]

{ #category : #'tests parsing' }
RBParserTest >> testNegativeNumberError [
	self should: [ self parserClass parseExpression: '- 2' ] raise: SyntaxErrorNotification
]

{ #category : #tests }
RBParserTest >> testNodesDo [
	| size |
	size := 0.
	self treeWithEverything nodesDo: [:e | size := size + 1].
	self assert: size = 19
]

{ #category : #'tests parsing' }
RBParserTest >> testNumberParsing [
	| numbers node |
	numbers := #(('1' 1) ('-1' -1) ('123' 123) ('123' 123) ('-123' -123) ('1.1' 1.1) ('-1.1' -1.1) ('1.23' 1.23) ('-1.23' -1.23) ('1e3' 1e3) ('1d3' 1d3) ('1q3' 1q3) ('-1e3' -1e3) ('1e-3' 1e-3) ('-1e-3' -1e-3) ('2r1e8'  2r1e8) ('-2r1e8' -2r1e8) ('2r1e-8' 2r1e-8) ('-2r1e-8' -2r1e-8) ('0.50s2' 0.50s2) ('0.500s3' 0.500s3) ('0.050s3' 0.050s3)).
	numbers do: [ :spec |
		node := self parserClass parseExpression: spec first.
		self assert: node source equals: spec first.
		self assert: node value equals: spec second ]
]

{ #category : #'tests parsing' }
RBParserTest >> testNumberRadixParsing [
	2 to: 32 do: [ :radix | 
		| radixString |
		radixString := radix printString, 'r'.
		0 to: 72 do: [ :i | 
			self  assert: (self parserClass parseExpression: (radixString, (i radix: radix)))
				value = i ] ]
]

{ #category : #tests }
RBParserTest >> testParents [
	(Array with: self treeWithEverything with: self treeWithReallyEverything) do: [ :tree |
		(Array with: tree with: tree copy) do: [ :root |
			root nodesDo: [ :node |
				node children do: [ :each |
					(each parent isMessage and: [ each parent isCascaded ]) ifFalse: [ 
						self assert: each parent == node.
						self assert: each methodNode == root ] ] ] ] ]
]

{ #category : #'tests parsing' }
RBParserTest >> testParseFaultyBlock [
	| node blockNoArgButBar blockNoArgButColon unfinishedBlock |
	"parsing block should mark this block as faulty, if it is unfinished no closing brace, or missing parts (colon but no arg, 
	no arg but vertical bar)"
	blockNoArgButBar := '[ | ]'.
	blockNoArgButColon := '[ : ]'.
	unfinishedBlock := '[  '.
	{blockNoArgButBar.
	blockNoArgButColon.
	unfinishedBlock}
		do: [ :faultyExpression | 
			node := self parseFaultyExpression: faultyExpression.
			self assert: node isBlock.
			self assert: node isFaulty]
]

{ #category : #'tests parsing' }
RBParserTest >> testParseFaultyLiteral [
	| faultyLiteral faultyLiteralArray node |
	"a literal or literal array object with an unknown character can be parse
	as faulty expression. If so, the error token (unknown character) needs to be consumed
	otherwise we get an endless loop"
	faultyLiteral := '¿'.
	faultyLiteralArray  := '#(¿)'.
	node := self parseFaultyExpression: faultyLiteral.
	self assert: node isFaulty.
	self assert: node isSequence.
	self assert: node statements first errorMessage equals: 'Unknown character'.	
	node := self parseFaultyExpression: faultyLiteralArray.
	self assert: node isFaulty.
	self assert: node isLiteralArray.	
	self assert: node contents first errorMessage equals: 'Unknown character'
]

{ #category : #'tests parsing' }
RBParserTest >> testParseFaultyMethod [
	| node strangeExpressions |
	
	strangeExpressions := OrderedCollection new .
	strangeExpressions 
		add: 'method: asd   self ,';
		add: 'method: asd   self a:'; "message without needed argument"
		add: 'method: asd   []]'; 
		add: 'method: asd  [  ] ,';"binary message without second argument"
		add: 'method:  ^^'; "returning twice"
		add: 'method: asd  ['; "opening a block"
		add: 'method: asd  ^ asd. n'; "invalid expresion after return"
		add: 'method: asd  ^ {'; "Only Open a literal array"
		add: 'selector '''; 
		add: 'selector #^';
		add: 'selector ¿'.


	strangeExpressions do: [ :exp | 
			node := self parseFaultyMethod: exp.
			self assert: node isMethod.
			self assert: node isFaulty
	].

	
]

{ #category : #'tests parsing' }
RBParserTest >> testParseFaultyMethodExpresionHasErrorNodeAsFinal [
	| node strangeExpression |
	
	strangeExpression := 'method: asd  ^ asd. n'.
	node := self parseFaultyMethod: strangeExpression.

	self assert: node isMethod.
	self assert: node isFaulty.
	self assert: node statements size equals: 2.
	self assert: node statements last isFaulty
	
]

{ #category : #'tests parsing' }
RBParserTest >> testParseFaultyMethodMessagePattern [
	| node faultyMessagePattern |
	faultyMessagePattern := '1'.
	node := self parseFaultyMethod: faultyMessagePattern.
	self assert: node isFaulty
]

{ #category : #'tests parsing' }
RBParserTest >> testParseMethodWithErrorTokenIsWellFormed [
	| node strangeMethod body statement message errorNode |
	
	strangeMethod := '
	selector  
		|temp| 
		temp := ''this is right'', ''wrong because no end.
		^temp'.
	node := self parseFaultyMethod: strangeMethod.

	self assert: node isMethod.
	self assert: node isFaulty.
	self assert: node arguments isEmpty.

	body := node body.
	self assert: body isSequence.
	self assert: body isFaulty.
	self assert: (body temporaries includes: (RBVariableNode named: 'temp')).

	statement := body statements first.
	self assert: statement isFaulty.
	self assert: statement isAssignment .
	
	message := statement value.
	self assert: message isFaulty.
	self assert: message arguments size equals: 1.

	errorNode := message arguments at: 1.
	self assert: errorNode isFaulty.
	self assert: errorNode value equals: 'wrong because no end.
		^temp'.
	self assert: errorNode errorMessage equals: 'Unmatched '' in string literal.' translated
	
	
	
]

{ #category : #'tests parsing' }
RBParserTest >> testParseUnfinishedBlockWithFaultyBody [
	| node unfinishedBlockExp missingArgExp unfinishedBlockAndMissingArgExp validBlock unfinishedBlockWithReturnExp|
	"Parsing a block as a faulty expression, where the block body is faulty and there is a missing closing bracket,
the whole expression is replaced by a single RBParseErrorNode, this makes it difficult for the highlighter to 
indicate the invalid expression."
	unfinishedBlockExp := '[:x | x missingArg: 3. '.
	missingArgExp := '[:x | x missingArg: . ]'.
	unfinishedBlockAndMissingArgExp := '[:x | x missingArg: . '.
	unfinishedBlockWithReturnExp := '[^ '.
	validBlock := '[:x | x missingArg: 3 ]. '.
	
	{unfinishedBlockExp. missingArgExp. unfinishedBlockAndMissingArgExp. unfinishedBlockWithReturnExp}
		do: [ :faultyExpression | 
			node := self parseFaultyExpression: faultyExpression.
			self assert: node isBlock.
			self assert: node isFaulty.
			self assert: node body isFaulty ].
		
	node := self parseFaultyExpression: validBlock.
	self assert: node isBlock.
	self deny: node isFaulty.
	self deny: node body isFaulty
]

{ #category : #'tests parsing' }
RBParserTest >> testParserErrors [
	#(#('self foo. + 3' 11) #('#(' 3) #('self 0' 6) #('self asdf;;asfd' 11)) 
		do: [:each | self parseError: each]
]

{ #category : #'tests parsing' }
RBParserTest >> testParsingLiteralMessages [
	self assert: (self parserClass parseExpression: 'nil self nil') isMessage.
	self assert: (self parserClass parseExpression: 'self true') isMessage.
	self assert: (self parserClass parseExpression: 'self false') isMessage.
	self assert: (self parserClass parseExpression: 'self -1') isMessage.
	
	self assert: (self parserClass parseMethod: 'nil') isMethod.
	self assert: (self parserClass parseMethod: 'true') isMethod.
	self assert: (self parserClass parseMethod: 'false') isMethod
]

{ #category : #tests }
RBParserTest >> testPositions [
	| blockNode |
	blockNode := self parserClass parseExpression: '[:a :b | ]'.
	self assert: blockNode left = 1.
	self assert: blockNode right = 10.
	self assert: blockNode bar = 8.
	self assert: blockNode sourceInterval = (1 to: 10).
	self assert: blockNode size = 1.	"test dummy collection protocol"
	blockNode printString.	"coverage"
	self deny: (blockNode isLast: (RBVariableNode named: 'b')).
	self compare: blockNode
		to: (RBBlockNode 
				arguments: (OrderedCollection with: (RBVariableNode named: 'a')
						with: (RBVariableNode named: 'b'))
				body: (RBSequenceNode statements: OrderedCollection new)).
				
]

{ #category : #'tests parsing' }
RBParserTest >> testPragmaConstantLiteralArgument [
	| primitiveDeclartion validPragma invalidPragma |
	"primitive declarations with variable (code) are allowed"
	primitiveDeclartion := 'foo 
	<primitive: #dooo module: #bar error: code>
	^ nil'.
	"#Point as argument is allowed"
	validPragma := 'foo 
	<return: #Point>
	^ 0@0'.
	"Point as argument is not allowed"
	invalidPragma := 'foo 
	<return: Point>
	^ 0@0'.
	self shouldnt:[self parserClass parseMethod: primitiveDeclartion] raise: Exception.
	self shouldnt:[self parserClass 	parseMethod: validPragma] raise: Exception.
	self should:[self parserClass parseMethod: invalidPragma] raise: Exception.
	

]

{ #category : #'tests parsing' }
RBParserTest >> testPragmaImplicitLiteralArrayIsInvalid [
	| pragmaWithExpressionAsArgument pragmaWithLiteralArrayAsArgument pragmaWithStringAsArgument |
	"a pragma with arguments that look like code
	(3+4) were implicitly converted to a literal array
	#(3+4), this is not allowed anymore"
	pragmaWithExpressionAsArgument := 'foo 
	<func: (3+4) res: 7>
	^ self'.
	self should:[self parserClass parseMethod: pragmaWithExpressionAsArgument] raise: Exception.

	"an explicit literal array is allowed"
	pragmaWithLiteralArrayAsArgument := 'foo 
	<func: #(3+4) res: 7>
	^ self'.
	"Should work"
	self parserClass parseMethod: pragmaWithLiteralArrayAsArgument.

	"and of course a string literal"
	pragmaWithStringAsArgument := 'foo 
	<func: ''(3+4)'' res: 7>
	^ self'.
	"should work"
	self parserClass parseMethod: pragmaWithStringAsArgument

]

{ #category : #'tests parsing' }
RBParserTest >> testPragmaPrimitiveError [
	| tree  pragma |
	tree := self parserClass 
				parseMethod: 'veryBasicAt: index 
	<primitive: #dooo module: #bar error: code>
	^ code'.
	
	self assert: tree pragmas size equals: 1.
	
	pragma := tree pragmas first.
	self assert: pragma selector equals: #primitive:module:error:
]

{ #category : #'tests parsing' }
RBParserTest >> testPragmas [
	| tree node |
	#(('foo <foo>' 				#foo ()) 
	('foo <foo: 1>' 				#foo: (1)) 
	('foo <foo: 1.2>' 			#foo: (1.2)) 
	('foo <foo: -3>' 			#foo: (-3)) 
	('foo <foo: #a>' 			#foo: (a)) 
	('foo <foo: #< >' 			#foo: (<)) 
	('foo <foo: #> >' 			#foo: (>)) 
	('foo <foo: $a>' 			#foo: ($a)) 
	('foo <foo: ''bar''>' 		#foo: ('bar')) 
	('foo <foo: true>' 			#foo: (true)) 
	('foo <foo: false>' 			#foo: (false)) 
	('foo <foo: nil>' 			#foo: (nil)) 
	('foo <foo: #(1 2)>' 		#foo: ((1 2)))
	('foo <foo: 1 bar: 2>' 		#foo:bar: (1 2))
	('foo |t| <foo: 1.2>' 		#foo: (1.2))
	('foo |t| <foo: 1 bar: 2>' #foo:bar: (1 2))
	) do: [ :each |
		tree := self parserClass parseMethod: each first.
		self assert: tree pragmas size equals: 1.
		node := tree pragmas first.
		self assert: node selector equals: each second.
		1 to: node arguments size do: [ :i |
			self assert: (node arguments at: i) value equals: (each last at: i) ].
		self assert: (each first at: node start) equals: $<.
		self assert: (each first at: node stop) equals: $> ].
	
	self assert: (self parserClass parseMethod: 'selector <a>') isMethod.
	self assert: (self parserClass parseMethod: '
	selector 
		"comment1" 
		|temp| 
		"comment2" 
		<a> 
		#<.
		^1') isMethod.
	self should: [self parserClass parseMethod: '
	selector 
		"comment1" 
		|temp| 
		"comment2" 
		<a> 
		<.
		^1'] raise: SyntaxErrorNotification
]

{ #category : #'tests parsing' }
RBParserTest >> testPrimitives [
	self assert: (Object parseTreeFor: #basicAt:) isPrimitive.
	#(('foo ^true' false ) ('foo <some: #tag> ^true' false ) (' foo <some: #tag> <primitive: 123> ^true' true ) ) 
			do: [:each | self assert: (self parserClass parseMethod: each first) isPrimitive equals: each last]
]

{ #category : #tests }
RBParserTest >> testQuerying [
	| tree aNode arg1Node bNode |
	tree := self parserClass 
				parseMethod: ('test: a`	| b |`	b := (self foo: a; bar) baz.`	b := super test: b.`	^[:arg1 | self foa1 + (super foo: arg1 foo: a foo: b)]' 
						copyReplaceAll: '`'
						with: (String with: (Character value: 13))).
	self 
		assert: tree selfMessages asSortedCollection asArray = #(#bar #foa1 #foo:).
	self assert: tree superMessages asSortedCollection asArray 
				= #(#foo:foo:foo: #test:).
	aNode := tree whichNodeIsContainedBy: (112 to: 112).
	self assert: aNode name = 'a'.
	bNode := tree whichNodeIsContainedBy: (119 to: 119).
	self assert: bNode name = 'b'.
	arg1Node := tree whichNodeIsContainedBy: (102 to: 105).
	self assert: arg1Node name = 'arg1'.
	self assert: (arg1Node statementNode isMessage 
				and: [arg1Node statementNode selector = #+]).
	self assert: (arg1Node whoDefines: 'arg1') isBlock.
	self assert: (aNode whoDefines: 'a') isMethod.
	self assert: (aNode whoDefines: 'b') isSequence.
	self assert: (tree whichNodeIsContainedBy: (91 to: 119)) selector 
				= #foo:foo:foo:.
	self assert: (tree whichNodeIsContainedBy: (69 to: 121)) isBlock.
	self assert: (tree whichNodeIsContainedBy: (69 to: 118)) isNil.
	self assert: aNode blockVariables asSortedCollection asArray = #('arg1').
	self assert: aNode temporaryVariables asSortedCollection asArray = #('b').
	self assert: tree allDefinedVariables asSortedCollection asArray 
				= #('a' 'arg1' 'b').
	self assert: tree allArgumentVariables asSortedCollection asArray 
				= #('a' 'arg1').
	self 
		assert: tree allTemporaryVariables asSortedCollection asArray = #('b')
]

{ #category : #tests }
RBParserTest >> testQueryingPrimitiveErrorVar [
	| tree |
	tree := self parserClass 
				parseMethod: 'clone
	"Answer a shallow copy of the receiver."
	<primitive: 148 error: ec>
	| copy |
	ec == #''insufficient object memory'' ifFalse:
		[|nothing| nothing := nil.
		^ noting].
   copy := nil.
	^ copy'.
	self assert: (tree body whoDefines:'copy') equals: tree body.
	self assert: (tree blockNodes first body whoDefines: 'nothing') equals: tree blockNodes first body.
	self assert: (tree body whoDefines: 'ec') equals: tree.

]

{ #category : #tests }
RBParserTest >> testReplacingNodes [
	| tree search block |
	tree := self parserClass 
				parseMethod: '+ a | a b | self ifTrue: [a] ifFalse: [b := c]. a := b. [:b :c :a | a foo: a; foo1: a; foo2: a foo: b]. {a. b}. ^a'.
	search := RBParseTreeSearcher new.
	block := [:aNode :answer | aNode replaceWith: (RBVariableNode named: 'q')].
	search
		matches: 'a' do: block;
		matchesArgument: 'a' do: block.
	search executeTree: tree.
	self 
		assert: tree = (self parserClass 
						parseMethod: '+ q | q b | self ifTrue: [q] ifFalse: [b := c]. q := b. [:b :c :q | q foo: q; foo1: q; foo2: q foo: b]. {q. b}. ^q').
	self 
		assert: tree removeDeadCode = (self parserClass 
						parseMethod: '+ q | q b | self ifTrue: [] ifFalse: [b := c]. q := b. {q. b}. ^q')
]

{ #category : #'tests parsing' }
RBParserTest >> testStatements [
	| tree |
	#(('' 0 0) ('.' 0 1) ('| bar |' 0 0) ('| bar | .' 0 1) ('| bar | ..' 0 2) ('foo. bar' 2 1) ('foo. bar.' 2 2) ('foo. bar. .' 2 3) ('. foo. bar' 2 2)) do: [ :each |
		tree := self parserClass parseExpression: each first.
		self assert: tree statements size equals: each second.
		self assert: tree periods size equals: each last ]
]

{ #category : #'tests parsing' }
RBParserTest >> testString [
	self assert: (self parserClass parseMethod: 'selector ^ ''<''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ^ ''<'', self') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''=''') isMethod.
	self assert: (self parserClass parseMethod: 'selector '':=''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''<''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''>''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ^ ''<>''') isMethod.
]

{ #category : #'tests parsing' }
RBParserTest >> testSymbolLiteral [
	| tree |
	#(
	'#<' #<
	'#>' #>
	'#<>' #<>
	'##foo' #foo 
	'###foo' #foo 
	'#foo:' #foo:
	'#foo::' #'foo::'
	'#foo::bar' #'foo::bar'
	'#foo::bar:' #'foo::bar:'
	'#foo::bar::' #'foo::bar::') pairsDo: [ :parseString :expectedResult |
		tree := self parserClass parseExpression: parseString.
		self assert: tree value equals: expectedResult.		
		self assert: tree start equals: 1.
		self assert: tree stop equals: parseString size ].
	
	#(
	('#"bar"foo' #foo)
	('# foo' #foo)
	('#1' 1)
	('#12' 12)
	('#12.3' 12.3)
	('# 1' 1)
	('##1' 1)
	('#"bar"1' 1)) do: [ :pair | 
		self should: [self parserClass parseExpression: pair first] raise: SyntaxErrorNotification ]
]

{ #category : #private }
RBParserTest >> treeWithEverything [
	^ RBParser parseMethod: 'method: arg1 | temps | temps := #(10). temps foo; foo. ^(temps collect: [:e | ])'
]

{ #category : #private }
RBParserTest >> treeWithReallyEverything [
	^ RBParser parseMethod: 'method: arg1 <foo> <foo: 1.2> | temps | temps := #[ 1 2 3 ]. temps := #(true false nil 1 1.2 $a foo #foo ''foo'' #() #(1 2) #[] #[1 2]). { 1 negated. 1 + 2. 1 raisedTo: 2 }. temps foo; foo: self. ^ (temps collect: [:e | | btemps | ((e isNil)) ])'
]
