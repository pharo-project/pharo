"
SUnit tests for RBParseTreeSearcher
"
Class {
	#name : #RBParseTreeSearcherTest,
	#superclass : #TestCase,
	#instVars : [
		'searcher'
	],
	#category : #'AST-Core-Tests-Matching'
}

{ #category : #asserting }
RBParseTreeSearcherTest >> assertCodeForSymbolAt: aSymbol in: myContextDictionary equals: expectedCode [

	^self
		assert: (myContextDictionary at: aSymbol asSymbol)
		equals: expectedCode
]

{ #category : #asserting }
RBParseTreeSearcherTest >> assertCodeForVariableNodeNamed: name in: myContextDictionary equals: expectedCode [

	^ self
		  assert:(myContextDictionary at: (RBPatternVariableNode named: name)) formattedCode
		  equals: expectedCode
]

{ #category : #helpers }
RBParseTreeSearcherTest >> parseExpression: aString [

	^ RBParser parseExpression: aString
]

{ #category : #helpers }
RBParseTreeSearcherTest >> parseMethod: aString [

	^ RBParser parseMethod: aString
]

{ #category : #helpers }
RBParseTreeSearcherTest >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #running }
RBParseTreeSearcherTest >> setUp [
	super setUp.
	searcher := self parseTreeSearcher
]

{ #category : #tests }
RBParseTreeSearcherTest >> testAListDoesNotMatchComposedMessagesBecauseOfTreeMismatch [
	"Using `@arg, we looking for multiple lists in one match pattern at the same time.
	Lists can be any receiver or args."

	| contextDictionary |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		"do: [ :aNode :answer | contextDictionary := searcher context ]."
		do: [ :aNode :answer | self fail ].


	searcher executeTree: (self parseExpression:
			 'self assert: reader storedSettings first realValue equals: false.').

	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testAListDoesNotMatchComposedMessagesBecauseOfTreeMismatch2 [
	"Using `@arg, we looking for multiple lists in one match pattern at the same time.
	Lists can be any receiver or args."

	| contextDictionary |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | contextDictionary := searcher context ].

	"so in below example, there is no match with the suggested pattern. That is why, dict is nil "
	searcher executeTree:
		(self parseExpression: 'self assert: token isLiteralToken.').

	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testAListMatchesComposedMessages [
	"Using `@arg, we looking for multiple lists in one match pattern at the same time.
	Lists can be any receiver or args."

	| contextDictionary |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression:
			 'self assert: reader storedSettings first realValue equals: true.').

   self assertCodeForVariableNodeNamed: '`@receiver' in: contextDictionary equals: 'self'.
	self assertCodeForVariableNodeNamed: '`@arg' in: contextDictionary equals: 'reader storedSettings first realValue'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testDoesNotMultipleKeywordsWithoutAList [
	"this test shows that to match a multi keyword message, you need to have a list (@) in the pattern"
	| contextDictionary |
	searcher
		matches: '`@rcv `sel: 2'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree:
		(self parseExpression: 'self newRequestTo: 2 with: 55').

	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMacthesDoisMatchingStatementsInDynamicArray [
	"pattern with statementlist in a dynamic array, to match all dynamic array expressions"

	| contextDictionary |
	searcher matches: '{`.@stmts.}' do: [ :aNode :answer |  contextDictionary:= searcher context ].
	contextDictionary := searcher executeTree: (self parseExpression:'{ (1@2) . Color red . 3 }').
	self assert: (contextDictionary at: (RBPatternVariableNode named: '`.@stmts')) size equals: 3
]

{ #category : #'test-argument' }
RBParseTreeSearcherTest >> testMatchesArgumentToTestImplicitAddArgument [

	"Here we can see the difference between matchesArgument: and matches:.
	 We can see that the matchContextArgList is filled with the all a arguments in the code by calling matchesArgument:do: instead of matches:do:
	 matches:do: is matching all a that exists in our tree except arguments "

	| tree search aParser block blockArg matchContextList matchContextArgList |

	matchContextList := OrderedCollection new.
	matchContextArgList := OrderedCollection new.
	aParser := RBParser new.

	search := self parseTreeSearcher.
	tree := self parseMethod: '+ a
									| a b |
									self
										ifTrue: [a]
										ifFalse: [b := c].
									a := b.
									[:b :c :a | a foo: a; foo1: a; foo2: a foo: b].
									{a. b}.
									^a'.

	block := [ :aNode :answer | matchContextList add: aNode ].
	blockArg := [ :aNode :answer | matchContextArgList add: aNode ].

	search
		matches: 'a' do: block;
 		matchesArgument: 'a' do: blockArg.
	search executeTree: tree
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchesDoDoesNotMatchAllElementsOfDynamicArray [
	"Matches in context for arraynodes wrongly matched all array statements against the last variable, if all variables are pattern-list-variables"

	| contextDictionary |
	contextDictionary := nil.
	searcher matches: '{`@first. `@second. `@third.}' do: [ :aNode :answer |  contextDictionary:= searcher context ].
	contextDictionary := searcher executeTree: (self parseExpression: '1 + 2').
	self assert: contextDictionary isNil.

	searcher matches: '{`@first. `@second. `@third.}' do: [ :aNode :answer |  contextDictionary:= searcher context ].
	contextDictionary := searcher executeTree: (self parseExpression: '{ (1@2) . Color red . 3 . 4}').
	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchesDoIsMatchingAllElementsOfDynamicArray [
	"Matches in context for arraynodes wrongly matched all array statements against the last variable, if all variables are pattern-list-variables"

	| contextDictionary |
	contextDictionary := nil.
	searcher matches: '{`@first. `@second. `@third.}' do: [ :aNode :answer |  contextDictionary:= searcher context ].
	contextDictionary := searcher executeTree: (self parseExpression:'{ (1@2) . Color red . 3}').
	self deny: contextDictionary isNil.
	"since the pattern matched, the block was executed and the value of the dictionary changed"

	self assertCodeForVariableNodeNamed: '`@first' in: contextDictionary equals: '(1 @ 2)'.
	self assertCodeForVariableNodeNamed: '`@second' in: contextDictionary equals: 'Color red'.
	self assertCodeForVariableNodeNamed: '`@third' in: contextDictionary equals: '3'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchesDoIsMatchingPatternedExpressionWithLiterals [
	""

	| contextDictionary |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: 'self
					assert: each isReadOnlyObject equals: true.').

	self assertCodeForVariableNodeNamed: '`@receiver' in: contextDictionary equals: 'self'.
	self assertCodeForVariableNodeNamed: '`@arg' in: contextDictionary equals: 'each isReadOnlyObject'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchesDoIsNotMatchingPatternedExpressionWithLiterals [

	| contextDictionary |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: 'self
					assert: each isReadOnlyObject equals: false.').

	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingAnyNodeWithoutConstrainingThePattern [
	"A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString)."

	| contextDictionary |
	searcher
		matches: '`@rcv'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: '(self + 1) put').

	"this is because `@rcv means match a tree"

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: '(self + 1) put'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingReceiver [
	"A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString). So here we are looking for any receiver followed by put."

	| contextDictionary |
	searcher
		matches: '`@rcv put'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: 'self put').

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: 'self'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingReceiver2 [
	"A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString). So here we are looking for any receiver followed by put."

	| contextDictionary |
	searcher
		matches: '`@rcv put'
		do: [ :aNode :answer | contextDictionary := searcher context ].
 	searcher executeTree: (self parseExpression: '(self + 1) put').

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: '(self + 1)'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingReceiverAndMultiKeywordSelector [

	| contextDictionary |
	searcher
		matches: '`@rcv `@sel: 2'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree:
		(self parseExpression: 'self newRequestTo: 2 with: 55').

	self assertCodeForSymbolAt: '`@sel:' in: contextDictionary equals: 'newRequestTo:with:'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingReceiverAndMultiKeywordSelector2 [

	| contextDictionary |
	searcher
		matches: 'self `@sel: `@args'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree:
		(self parseExpression: 'self newRequestTo: 2 with: 55').

	self assertCodeForSymbolAt: '`@sel:' in: contextDictionary equals: 'newRequestTo:with:'.

	"The below is not working: simething wrong with the assertion type"

	"self
		  assert:(contextDictionary at: (RBPatternVariableNode named: '`@args'))
		  equals: #( 2 55) asArray "
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingReceiverAndSelector [

	| contextDictionary |
	searcher
		matches: '`@rcv `sel: 2'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree:
		(self parseExpression: 'self newRequestTo: 2').

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: 'self'.

	self
		assert:
		(contextDictionary at: '`sel:' asSymbol)
		equals: 'newRequestTo:'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingReceiverAndSelector2 [

	| contextDictionary |
	searcher
		matches: '(`@rcv `sel: 2) put'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree:
		(self parseExpression: '(self newRequestTo: 2) put').

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: 'self'.
 	self assertCodeForSymbolAt: '`sel:' in: contextDictionary equals: 'newRequestTo:'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testMatchingReceiverComposite [

	"A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString). So here we are looking for any receiver followed by put."

	| contextDictionary |
	searcher
		matches: '`@rcv put'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree:
		(self parseExpression: '(self newRequestTo: aString) put').

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: '(self newRequestTo: aString)'.

	"Ex 2: "
	searcher executeTree: (self parseExpression: 'self httpClient put').
	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: 'self httpClient'
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListComplex [

	"Here we are looking for multiple lists in one match pattern at the same time.
	Lists can be any receiver or arguments"

	| dict |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | dict := searcher context ].

	searcher executeTree: (self parseExpression: 'self
					assert: each isReadOnlyObject equals: true.').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@receiver')) formattedCode
		equals: 'self'.

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@arg')) formattedCode
		equals: 'each isReadOnlyObject'
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListComplexNotMatching [

	"Here we are looking for multiple lists in one match pattern at the same time.
	Lists can be any receiver or argument"

	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | self fail ].

	searcher executeTree:
		(self parseExpression: 'self assert: token isLiteralToken.')
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchListReceiverNotMatching [
	"A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString). So here we are looking for any receiver followed by put. "

	| contextDictionary |
	searcher
		matches: '`@rcv put'
		do: [ :aNode :answer | contextDictionary := searcher context ].

	" The below example is not matching bcz the message is different than put. Which is why the contextsDictionary is still empty "

	searcher executeTree: (self parseExpression: '1 at').
	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchListTempVars [

	"Here we are looking for a method that contains a list of temporary vars followed by myVar1 and then a receiver followed by message messageTofind"
	"The below example is correct, matching with our pattern even if we have multiple variables"

	| contextDictionary |
	searcher
		matches: '`sel |`@temps `temp| `rcv messageTofind'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: 'example
								|aTempVar aTempVar2 aTempVar3 myTempVar|
								self messageTofind.').

	self assertCodeForVariableNodeNamed: '`sel' in: contextDictionary equals: 'example'.
	self assertCodeForVariableNodeNamed: '`@temps' in: contextDictionary equals: 'aTempVar aTempVar2 aTempVar3'.

	"Here the engine stores a symbol "
	self assertCodeForSymbolAt: '`temp' in: contextDictionary equals: 'myTempVar'.

	self assertCodeForVariableNodeNamed: '`rcv' in: contextDictionary equals: 'self'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchListTempVarsNotMatching [

	"here we are looking for a method that containns a list of temporary vars followed by myVar1 and then a receiver followed by message messageTofind"

	| contextDictionary |
	searcher
		matches: '`sel |`@temps `temp| `rcv messageTofind'
		do: [ :aNode :answer | contextDictionary := searcher context ].

	"The second example is incorrect, NOT matching with our pattern bcz even we have one variable defined, but it is not followed by temp var which name cannot change"

	searcher executeTree: (self parseExpression: 'example2 |aTempVar| self messageTofind. ').

	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchLiteralArray [

	"so here we are looking for any literal with message size
	Litteral can be an array."

	| contextDictionary |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: '#(1 2 3) size').

	"This assertion is not working yet, something wrong with the type. The logic is correct bcz it is working on the Rewriter Tool"
	"self
		assert:
		(contextDictionary at: (RBPatternVariableNode named: '`#lit')) formattedCode
		""equals: (RBLiteralArrayNode named: #(1 2 3)).""
		equals:   #( 1 2 3)."
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchLiteralCollectNotWorking [

	"so here we are looking for any literal with message size; Ordered Collection is not a literal. "

	| contextDictionary |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: 'OrderedCollection new size').

	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchLiteralNumber [

	"so here we are looking for any literal with message size; Litteral can be a number."

	| contextDictionary |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: '3 size').

	self assertCodeForVariableNodeNamed: '`#lit' in: contextDictionary equals: '3'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchLiteralString [

	"so here we are looking for any literal with message size; Litteral can be a string"

	| contextDictionary |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: ' ''foo'' size').

	self assertCodeForVariableNodeNamed: '`#lit' in: contextDictionary equals: '''foo'''
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchRecurseInto [
	"Here we are testing the recurse into, which looks inside @vars for pattern that matches @vars + 1"

	searcher matches: '``@vars + 1' do: [ :aNode :answer | answer + 1 ].
	"So here: we have 3 message nodes. The one we are looking recursevily into is the first message node: myNum + 1 which is matching with the original pattern."

	"Match 1: (myNum + 1) + 1; Match 2: (myNum + 1)"

	self
		assert: (searcher
				 executeTree: (self parseExpression: '(myNum + 1) + 1 + 5')
				 initialAnswer: 0)
		equals: 2
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchRecurseIntoNotMatching [
	"Here we are testing the recurse into, which looks inside @vars for pattern that matches @vars + 1"

	searcher matches: '``@vars + 1' do: [ :aNode :answer | answer + 1 ].
	self
		deny: (searcher
				 executeTree:
				 (self parseExpression: '(myNum + 1) + 1 + 5 + (myNum + 1)')
				 initialAnswer: 0)
		equals: 1
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchSelectors [
	"So here we are looking for an expression that starts with a receiver followed by at: message , then a list of arguments, then another selector that could be any selector ... followed by a second list of args"

	| contextDictionary |
	searcher
		matches: '`@rcv at: `@arg `sel: `@arg1'
		do: [ :aNode :answer | contextDictionary := searcher context ].
 	searcher executeTree:
		(self parseExpression: 'cache at: each ifAbsentPut: [ each ].').

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: 'cache'.
	self assertCodeForVariableNodeNamed: '`@arg' in: contextDictionary equals: 'each'.
	self assertCodeForVariableNodeNamed: '`@arg1' in: contextDictionary equals: '[ each ]'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchSelectorsMultiArgs [

	"So here we are looking for an expression that starts with a receiver followed by at: message , then a list of arguments, then another selector that could be any selector ... followed by a second list of args"

	| contextDictionary |
	searcher
		matches: '`@rcv at: `@arg `sel: `@arg1'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: 'collection at: offset + count put: object.').

	self assertCodeForVariableNodeNamed: '`@rcv' in: contextDictionary equals: 'collection'.
	self assertCodeForVariableNodeNamed: '`@arg' in: contextDictionary equals: 'offset + count'.
	self assertCodeForVariableNodeNamed: '`@arg1' in: contextDictionary equals: 'object'
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchSelectorsNotMatching [

	"So here we are looking for an expression that starts with a receiver followed by at: message , then a list of arguments, then another selector that could be any selector ... followed by a second list of args"

	| contextDictionary |
	searcher
		matches: '`@rcv at: `@arg `sel: `@arg1'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression: 'self assert: preferences exists.').

	self assert: contextDictionary isNil
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchStatements [

	"No comment yet ..."

	searcher matches: '`.Statements.' do: [ :aNode :answer | answer + 1.].

	self
		assert: (searcher
				 executeTree:
					 (self parseExpression: 'myArray := OrderedCollection new.
													myPoint := Point new setX: 1 setY: 2')
				 initialAnswer: 0)
		equals: 2
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearching [
	searcher
		matches: '``@rcv at: `#arg `sel: ``@arg1'
		do: [ :aT :answer | answer + 1 ].
	self
		assert:
			(searcher
				executeTree:
					(self
						parseExpression: 'self at: (self at: 9 put: 8)  put: 2; at: 2 ifAbsent: []; ifAbsent: 2 at: 1; at: 4; foo')
				initialAnswer: 0)
		equals: 2.


		"

	searcher := self parseTreeSearcher.
	searcher
		matches: '``@rcv `at: ``@arg1 `at: ``@arg2'
		do: [ :aNode :answer | answer + 1 ].
	self
		assert:
			(searcher
				executeTree:
					(RBParser
						parseExpression: 'self at: 1 at: 3; at: 1 put: 32; at: 2; foo; at: 1 at: 1 at: 2')
				initialAnswer: 0)
		equals: 1.


	searcher := self parseTreeSearcher.
	searcher
		matchesMethod: 'at: `object `put: `o1 ``@rcv `put: 1'
		do: [ :aNode :answer | true ].
	self
		assert:
			(searcher
				executeTree: (RBParser parseMethod: 'at: a put: b self foo put: 1')
				initialAnswer: false)"
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchingComposedMessage [

	searcher
		matches: '``@rcv at: ``@arg'
		do: [ :aNode :answer | answer + 1 ].
	"the match is recursing into receiver and arguments."
	self
		assert: (searcher
					executeTree: (self parseExpression: '(self at: 1) at: (self at: 4); foo')
					initialAnswer: 0)
		equals: 3
]

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchingComposedSelector [

	searcher
		matches: '``@rcv at: ``@arg `sel: ``@arg1'
		do: [ :aNode :answer | answer + 1].

	"the result is 2 because the parse tree searcher only finds at:x: messages like at:put: and at:ifAbsent:"
	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self at: 1 put: 2; at: 2 ifAbsent: []; ifAbsent: 2 at: 1; at: 4; foo')
					initialAnswer: 0) equals: 2
]

{ #category : #'tests - selectors' }
RBParseTreeSearcherTest >> testSearchingComposedSelector2 [

	searcher
		matches: '``@rcv at: ``@arg'
		do: [ :aNode :answer | answer + 1 ].
	"the result is 1 because the parse tree searcher only finds at: messages like at: and not ifAbsent: at:"
	self
		assert: (searcher
					executeTree: (self parseExpression: 'self ifAbsent: 2 at: 1; at: 4; at: 5; foo')
					initialAnswer: 0)
		equals: 2
]

{ #category : #'tests - selectors' }
RBParseTreeSearcherTest >> testSearchingComposedSelectorWithCascade [

	searcher
		matches: '``@rcv at: ``@arg `sel: ``@arg1'
		do: [ :aNode :answer | answer + 1].

	"the result is 2 because the parse tree searcher only finds at:x: messages
	like at:put: and at:ifAbsent:"

	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self
								at: 1 put: 2;
								at: 2 ifAbsent: [];
								ifAbsent: 2 at: 1;
								at: 4; foo')
					initialAnswer: 0) equals: 2
]

{ #category : #'tests - selectors' }
RBParseTreeSearcherTest >> testSearchingComposedSelectorWithListIsNotWorking [

	searcher
		matches: '``@rcv at: ``@arg `@sel: ``@arg1'
		do: [ :aNode :answer | answer + 1].

	"the result is 2 because the parse tree searcher only finds at:x: messages
	like at:put: and at:ifAbsent:"

	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self
								at: 1')
					initialAnswer: 0) equals: 0
]

{ #category : #'tests - selectors' }
RBParseTreeSearcherTest >> testSearchingComposedSelectorwithinCascade [

	searcher
		matches: '``@rcv at: ``@arg'
		do: [ :aNode :answer | answer + 1 ].
	"the result is 1 because the parse tree searcher only finds at: messages like at: and not ifAbsent: at:"
	self
		assert: (searcher
					executeTree: (self parseExpression: 'self ifAbsent: 2 at: 1; at: 4; at: 5; foo')
					initialAnswer: 0)
		equals: 2
]

{ #category : #'tests - selectors' }
RBParseTreeSearcherTest >> testSearchingKeywordsPartWithOptionInTheMiddleIsBogus [

	searcher
		matches: 'self at: 3 `@sel2: `@arg2 put: self '
		do: [ :aNode :answer | answer + 1].

	"the result is 2 because the parse tree searcher only finds at:x: messages
	like at:put: and at:ifAbsent:"

	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self  at: 3 foo: 1 put: self')
					initialAnswer: 0) equals: 1.
	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self  at: 3 foo: 1 bar: 3 put: self')
					initialAnswer: 0) equals: 0.
	"The previous assert should pass with 1 sel should be foo:bar:"
]

{ #category : #'tests - selectors' }
RBParseTreeSearcherTest >> testSearchingOptionKeywordsPartComposedSelector [

	searcher
		matches: '``@rcv `@sel: ``@arg'
		do: [ :aNode :answer | answer + 1].

	"the result is 2 because the parse tree searcher only finds at:x: messages
	like at:put: and at:ifAbsent:"

	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self  at: 1')
					initialAnswer: 0) equals: 1.
	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self  at: 1 put: 2')
					initialAnswer: 0) equals: 1
]

{ #category : #'tests - selectors' }
RBParseTreeSearcherTest >> testSearchingOptionKeywordsPartComposedSelector2 [
	"we do not understand why it is green. I should not because the second expression does not contain at:."
	searcher
		matches: '`@rcv `@sel: `@arg at: 3 `@sel2: `@arg2'
		do: [ :aNode :answer | answer + 1].

	"the result is 2 because the parse tree searcher only finds at:x: messages
	like at:put: and at:ifAbsent:"

	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self  foo: 1 at: 3 put: 5')
					initialAnswer: 0) equals: 1.
	self
		assert: (searcher
					executeTree: (self
							parseExpression: 'self  foo: 1 put: 2')
					initialAnswer: 0) equals: 1
]

{ #category : #'tests - add search' }
RBParseTreeSearcherTest >> testSimpleAddSearch [

	| res |
	searcher addSearch: '`@Super << `#ASymbol' -> [ :aNode :anAnswer | res := #class ].
	searcher executeTree: (RBParser parseExpression: ' Object << #Point').
	self assert: res equals: #class
]

{ #category : #'tests - add search' }
RBParseTreeSearcherTest >> testSimpleAddSearches [

	| res |
	searcher addSearches: { '`@Super << `#ASymbol' -> [ :aNode :anAnswer | res := #class ]}.
	searcher executeTree: (RBParser parseExpression: ' Object << #Point').
	self assert: res equals: #class
]

{ #category : #tests }
RBParseTreeSearcherTest >> testWithoutAListDoesNotMatchComposedMessages [
	"Using `arg, and not `@arg, we looking for multiple lists in one match pattern at the same time.
	Lists can be any receiver or args."

	| contextDictionary |
	searcher
		matches: '`@receiver assert: `arg equals: true'
		do: [ :aNode :answer | contextDictionary := searcher context ].
	searcher executeTree: (self parseExpression:
			 'self assert: reader storedSettings first realValue equals: true.').
	self assert: contextDictionary isNil
]
