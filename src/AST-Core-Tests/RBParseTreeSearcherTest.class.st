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

{ #category : #helpers }
RBParseTreeSearcherTest >> parseExpression: aString [ 

	^ RBParser parseExpression: aString
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #running }
RBParseTreeSearcherTest >> setUp [
	super setUp.
	searcher := self parseTreeSearcher
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testDynamicArrayStatements [
	"match in context for arraynodes wrongly matched all array statements against the last variable, if all variables are pattern-list-variables"

	| dict |
	searcher matches: '{`@first. `@second. `@third.}' do: [ :aNode :answer |  dict:= searcher context ].
	dict := searcher executeTree: (self parseExpression:'{ (1@2) . Color red . 3}').
	self assert: (dict at: (RBPatternVariableNode named:'`@first')) formattedCode equals: '(1 @ 2)'.
	self assert: (dict at: (RBPatternVariableNode named:'`@second')) formattedCode equals: 'Color red'.
	self assert: (dict at: (RBPatternVariableNode named:'`@third')) formattedCode equals: '3'
		
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testDynamicArrayWithStatementListPattern [
	"pattern with statementlist in a dynamic array, to match all dynamic array expressions"

	| dict |
	searcher matches: '{`.@stmts.}' do: [ :aNode :answer |  dict:= searcher context ].
	dict := searcher executeTree: (self parseExpression:'{ (1@2) . Color red . 3 }').	
	"self halt."
	self assert: (dict at: (RBPatternVariableNode named: '`.@stmts')) size equals: 3

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
RBParseTreeSearcherTest >> testSearchListComplexMultiArgs [

	"		
		- Here we looking for multiple lists in one match pattern at the same time. 
		- Lists can be any receiver or args
	"

	| dict |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | dict := searcher context ].

	searcher executeTree: (self parseExpression:
			 'self assert: reader storedSettings first realValue equals: true.').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@receiver')) formattedCode
		equals: 'self'.

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@arg')) formattedCode
		equals: 'reader storedSettings first realValue'
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListComplexNotMatching [

	"		
		- Here we looking for multiple lists in one match pattern at the same time. 
		- Lists can be any receiver or args
	"

	| dict |
	searcher
		matches: '`@receiver assert: `@arg equals: true'
		do: [ :aNode :answer | dict := searcher context ].

	"so in below example, there is no match with the suggested pattern. Which is why, dict is nil "

	searcher executeTree:
		(self parseExpression: 'self assert: token isLiteralToken.').

	self assert: dict isNil
	"self deny: dict isNotNil"
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListReceiver [

	"		
		- A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString) 
		- So here we are looking for any receiver followed by put.
	"

	| dict |
	searcher
		matches: '`@rcv put'
		do: [ :aNode :answer | dict := searcher context ].
 
	searcher executeTree: (self parseExpression: 'self put').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@rcv')) formattedCode
		equals: 'self'
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListReceiverMultiRcv [

	"		
		- A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString) 
		- So here we are looking for any receiver followed by put.
	"

	| dict |
	searcher
		matches: '`@rcv put'
		do: [ :aNode :answer | dict := searcher context ].
 
	searcher executeTree:
		(self parseExpression: '(self newRequestTo: aString) put').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@rcv')) formattedCode
		equals: '(self newRequestTo: aString)'.

	"Ex 2: "
	searcher executeTree: (self parseExpression: 'self httpClient put').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@rcv')) formattedCode
		equals: 'self httpClient'
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListReceiverNotMatching [

	"		
		- A receiver list can be: a simple receiver like self, or a complex receiver like (self newRequestTo: aString) 
		- So here we are looking for any receiver followed by put.
	"

	| dict |
	searcher
		matches: '`@rcv put'
		do: [ :aNode :answer | dict := searcher context ].
 
	" The below example is not matching bcz the message is different than put. Which is why the dict is still empty "

	searcher executeTree: (self parseExpression: '1 at').
	self assert: dict isNil
	"deny: dict isNotNil."
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListTempVars [

	"		
		- here we are looking for a method that containns a list of temporary vars followed by myVar1 and then a receiver followed by message messageTofind
	"

	| dict |
	searcher
		matches: '`sel |`@temps `temp| `rcv messageTofind'
		do: [ :aNode :answer | dict := searcher context ].
 
	"Ex 1: The below example is correct, matching with our pattern even if we have multiple variables"

	searcher executeTree: (self parseExpression: 'example
								|aTempVar aTempVar2 aTempVar3 myTempVar|
								self messageTofind.').
	self
		assert:
		(dict at: (RBPatternVariableNode named: '`sel')) formattedCode
		equals: 'example'.

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@temps')) formattedCode
		equals: 'aTempVar aTempVar2 aTempVar3'.

	" !!! Not sure why below assert is not working properly, while on rewriter tool, the example is matching successfully !!!"

	"self
		assert:
		(dict at: (RBPatternVariableNode named: '`temp')) formattedCode
		equals: 'myVar'."

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`rcv')) formattedCode
		equals: 'self'
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchListTempVarsNotMatching [

	"		
		- here we are looking for a method that containns a list of temporary vars followed by myVar1 and then a receiver followed by message messageTofind
	"

	| dict |
	searcher
		matches: '`sel |`@temps `temp| `rcv messageTofind'
		do: [ :aNode :answer | dict := searcher context ].
 
	"The second example is incorrect, NOT matching with our pattern bcz even we have one variable defined, but it is not followed by temp var which name cannot change."

	searcher executeTree: (self parseExpression: 'example2
							|aTempVar|
							self messageTofind. ').

	self assert: dict isNil "
	deny: dict isNotNil."
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchLiteralArray [

	"		
		- so here we are looking for any literal with message size
	"

	| dict |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | dict := searcher context ]
		
	" Litteral can be an array.
		!!! Not sure why the example is not working, while it is working on the matching window of the rewrite tool !!!" "searcher executeTree: (self parseExpression: '#(1 2 3) size').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`#lit')) formattedCode
		equals: '#(1 2 3)'." 
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchLiteralCollectNotWorking [

	"		
		- so here we are looking for any literal with message size
	"

	| dict |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | dict := searcher context ].
 
	" Ordered Collection is not a literal."
	searcher executeTree:
		(self parseExpression: 'OrderedCollection new size').

	self
		assert: dict isNil .
		"deny: dict isNotNil ."
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchLiteralNumber [

	"		
		- so here we are looking for any literal with message size
	"

	| dict |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | dict := searcher context ].
 
	" Litteral can be a number "
	searcher executeTree: (self parseExpression: '3 size').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`#lit')) formattedCode
		equals: '3'.
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchLiteralString [

	"		
		- so here we are looking for any literal with message size
	"

	| dict |
	searcher
		matches: '`#lit size'
		do: [ :aNode :answer | dict := searcher context ].

	" Litteral can be a string "
	searcher executeTree: (self parseExpression: ' ''foo'' size').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`#lit')) formattedCode
		equals: '''foo'''.
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchRecurseInto [

	"		
		- Here we are testing the recurse into, which looks inside @vars for pattern that matches @vars + 1
	"

	searcher matches: '``@vars + 1' do: [ :aNode :answer | answer + 1 ].
 
	"	
		- So here: we have 3 message nodes 
		- The one we are looking recursevily into is the first message node: myNum + 1 which is matching with the original pattern.
	"
	
	"
		Match 1: (myNum + 1) + 1
		Match 2: (myNum + 1)
	"
			
	self
		assert: (searcher
				 executeTree: (self parseExpression: '(myNum + 1) + 1 + 5')
				 initialAnswer: 0)
		equals: 2.

	" ****** Deny ******"

	"Should be equal 3
		Match 1: (myNum + 1) + 1 
		Match 2: (myNum + 1)
		Match 3: (myNum + 1)
	"

	self
		deny: (searcher
				 executeTree:
				 (self parseExpression: '(myNum + 1) + 1 + 5 + (myNum + 1)')
				 initialAnswer: 0)
		equals: 1
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchRecurseIntoNotMatching [

	"		
		- Here we are testing the recurse into, which looks inside @vars for pattern that matches @vars + 1
	"

	searcher matches: '``@vars + 1' do: [ :aNode :answer | answer + 1 ].
 
	"Should be equal 3
		Match 1: (myNum + 1) + 1 
		Match 2: (myNum + 1)
		Match 3: (myNum + 1)
	"

	self
		deny: (searcher
				 executeTree:
				 (self parseExpression: '(myNum + 1) + 1 + 5 + (myNum + 1)')
				 initialAnswer: 0)
		equals: 1
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchSelectors [

	"		
		- So here we are looking for an expression that starts with a receiver followed by at: message , then a list of arguments, then another selector that could be any selector ... followed by a second list of args
	"

	| dict |
	searcher
		matches: '`@rcv at: `@arg `sel: `@arg1'
		do: [ :aNode :answer | dict := searcher context ].
 	searcher executeTree:
		(self parseExpression: 'cache at: each ifAbsentPut: [ each ].').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@rcv')) formattedCode
		equals: 'cache'.

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@arg')) formattedCode
		equals: 'each'.

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@arg1')) formattedCode
		equals: '[ each ]'.

	
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchSelectorsMultiArgs [

	"		
		- So here we are looking for an expression that starts with a receiver followed by at: message , then a list of arguments, then another selector that could be any selector ... followed by a second list of args
	"

	| dict |
	searcher
		matches: '`@rcv at: `@arg `sel: `@arg1'
		do: [ :aNode :answer | dict := searcher context ].
 
	searcher executeTree:
		(self parseExpression: 'collection at: offset + count put: object.').

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@rcv')) formattedCode
		equals: 'collection'.

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@arg')) formattedCode
		equals: 'offset + count'.

	self
		assert:
		(dict at: (RBPatternVariableNode named: '`@arg1')) formattedCode
		equals: 'object'.
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchSelectorsNotMatching [

	"		
		- So here we are looking for an expression that starts with a receiver followed by at: message , then a list of arguments, then another selector that could be any selector ... followed by a second list of args
	"

	| dict |
	searcher
		matches: '`@rcv at: `@arg `sel: `@arg1'
		do: [ :aNode :answer | dict := searcher context ].
 
	searcher executeTree:
		(self parseExpression: 'self assert: preferences exists.').

	self assert: dict isNil. 
	"deny: dict isNotNil"
]

{ #category : #'tests - to de refined' }
RBParseTreeSearcherTest >> testSearchStatements [

	"
		- No comment yet ...
	"

	searcher matches: '`.Statements.' do: [ :aNode :answer | 
		answer + 1.].
 
	self
		assert: (searcher
				 executeTree:
					 (self parseExpression: 'myArray := OrderedCollection new. 
													myPoint := Point new setX: 1 setY: 2')
				 initialAnswer: 0)
		equals: 2
]

{ #category : #'tests - to de refined' }
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

{ #category : #tests }
RBParseTreeSearcherTest >> testSearchingComposedSelector2 [

	searcher
		matches: '``@rcv at: ``@arg'
		do: [ :aNode :answer | answer + 1 ].
	"the result is 1 because the parse tree searcher only finds at: messages like at: and not ifAbsent: at:"	
	self 
		assert: (searcher 
					executeTree: (self parseExpression: 'self ifAbsent: 2 at: 1; at: 4; foo')
					initialAnswer: 0)
		equals: 1
]
