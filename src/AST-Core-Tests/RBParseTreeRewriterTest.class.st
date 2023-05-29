"
SUnit tests for RBParseTreeRewriter
"
Class {
	#name : #RBParseTreeRewriterTest,
	#superclass : #TestCase,
	#instVars : [
		'rewriter'
	],
	#category : #'AST-Core-Tests-Matching'
}

{ #category : #utilities }
RBParseTreeRewriterTest >> compare: anObject to: anotherObject [
	self assert: anObject hash equals: anotherObject hash.
	self assert: anObject equals: anotherObject
]

{ #category : #helpers }
RBParseTreeRewriterTest >> parseExpression: aString [
	^ self parserClass parseExpression: aString
]

{ #category : #helpers }
RBParseTreeRewriterTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #helpers }
RBParseTreeRewriterTest >> parseRewriteExpression: aString [
	^ self parserClass parseRewriteExpression: aString
]

{ #category : #helpers }
RBParseTreeRewriterTest >> parseRewriteMethod: aString [
	^ self parserClass parseRewriteMethod: aString
]

{ #category : #helpers }
RBParseTreeRewriterTest >> parserClass [
	^ RBParser
]

{ #category : #running }
RBParseTreeRewriterTest >> setUp [
	super setUp.
	rewriter := RBParseTreeRewriter new
]

{ #category : #'tests - ok' }
RBParseTreeRewriterTest >> testBlockRewrites [
	"This test shows that several rules can be used to specify different rewrite actions: the location in the tree structure, simple expression (self foo) using the node of the expression itself, and depending on the node kinds."

	| tree |
	tree := self treeToBeRewritten.
	"Here the rule says that we only replace in the rightmost children of the return node."
	rewriter replace: 'asdf' with: 'fdsa' when: [ :aNode | aNode parent parent isReturn ].
	"here we want the replace self foo by the value the selector of the self foo expression, i.e. foo"
	rewriter replace: 'self foo' withValueFrom: [ :aNode | RBVariableNode named: aNode selector asString ].
	"here the condition is false so the rule is not executed."
	rewriter replaceArgument: 'asdf' withValueFrom: [ :aNode | RBVariableNode named: 'xxx' ] when: [ :aNode | true ].
	rewriter executeTree: tree.
	self
		compare: tree
		to:
			(self
				parseMethod:
					'method: xxx
	<primitive: 1>
	<primitive: 2>
	^asdf +  foo + fdsa')
]

{ #category : #'tests - ok' }
RBParseTreeRewriterTest >> testBlockRewritesAreNotChained [
	"This test shows that rewrite rules are not chained sequentially. One is applied and this is it."

	| tree |
	tree := self treeToBeRewritten.
	"asdf ->  fdsa but not fdsa -> grgrgrgrgr"
	rewriter replace: 'asdf' with: 'fdsa' when: [ :aNode | true ].
	rewriter replace: 'fdsa' with: 'grgrgrgrgr' when: [ :aNode | true ].
	rewriter executeTree: tree.
	self
		compare: tree
		to:
			(self
				parseMethod:
					'method: asdf
	<primitive: 1>
	<primitive: 2>
	^fdsa +  self foo + fdsa')
]

{ #category : #'tests - ok' }
RBParseTreeRewriterTest >> testBlockRewritesArguments [
	"this test just shows that all the arguments are replaced. Check in contrast with testBlockRewritesArgumentsTakeIntoAccountConditions"
	| tree |
	tree := self
				parseMethod: 'method: asdf bar: bar
	<primitive: 1>
	<primitive: 2>
	^asdf + self foo + asdf'.

	rewriter
		replaceArgument: 'asdf'
		withValueFrom: [ :aNode | RBVariableNode named: 'xxx' ]
		when: [ :aNode | true ].

	rewriter
		replaceArgument: 'bar'
		withValueFrom: [ :aNode | RBVariableNode named: 'yyy' ]
		when: [ :aNode | true ].

	rewriter executeTree: tree.
	self compare: tree
		to: (self
				parseMethod: 'method: xxx bar: yyy
	<primitive: 1>
	<primitive: 2>
	^asdf +  self foo + asdf')
]

{ #category : #'tests - ok' }
RBParseTreeRewriterTest >> testBlockRewritesArgumentsTakeIntoAccountConditions [
	"this test shows that the condition controls the rewriting on the terms: here the bar argument is not rewritten because the condition is set to false."
	| tree |
	tree := self
				parseMethod: 'method: asdf bar: bar
	<primitive: 1>
	<primitive: 2>
	^asdf + self foo + asdf'.
	rewriter
		replaceArgument: 'asdf'
		withValueFrom: [ :aNode | RBVariableNode named: 'xxx' ]
		when: [ :aNode | true ].
	rewriter
		replaceArgument: 'bar'
		withValueFrom: [ :aNode | RBVariableNode named: 'yyy' ]
		when: [ :aNode | false ].

	rewriter executeTree: tree.
	self compare: tree
		to: (self
				parseMethod: 'method: xxx bar: bar
	<primitive: 1>
	<primitive: 2>
	^asdf +  self foo + asdf')
]

{ #category : #'tests - ok' }
RBParseTreeRewriterTest >> testBlockRewritesFirstRuleTakePrecedence [
	| tree |
	tree := self treeToBeRewritten.
	"Here the rule says that we only replace in the rightmost children of the return node."
	rewriter replace: 'asdf' with: 'fdsa' when: [ :aNode | true ].
	rewriter replace: 'asdf' with: 'grgrgrgrgr' when: [ :aNode | true ].
	rewriter executeTree: tree.
	self
		compare: tree
		to:
			(self
				parseMethod:
					'method: asdf
	<primitive: 1>
	<primitive: 2>
	^fdsa +  self foo + fdsa')
]

{ #category : #'tests - ok' }
RBParseTreeRewriterTest >> testBlockRewritesWithTrueConditionIsNotExecutedWhenNotMatchingCorrectNode [
	"This test shows that even if the condition of the rule is true, it will not be applied on inadequate nodes.
	Here, replaceArgument: is not for plain variable but only method arguments."

	| tree |
	tree := self treeToBeRewritten.
	rewriter replaceArgument: 'asdf' withValueFrom: [ :aNode | RBVariableNode named: 'xxx' ] when: [ :aNode | true ].
	rewriter executeTree: tree.
	self
		compare: tree
		to:
			(self
				parseMethod:
					'method: xxx
	<primitive: 1>
	<primitive: 2>
	^asdf +  self foo + asdf')
]

{ #category : #'tests - to be refined' }
RBParseTreeRewriterTest >> testMultimatch [
	| count |
	count := 0.
	rewriter
		replace: '``@object at: ``@foo'
		with: '``@object foo: ``@foo'
		when: [:aNode | (count := count + 1) == 2].
	self compare: (rewriter
				executeTree: (self parseExpression: 'self at: (bar at: 3)');
				tree)
		to: (self parseExpression: 'self at: (bar foo: 3)')
]

{ #category : #'tests - to be refined' }
RBParseTreeRewriterTest >> testPatternCascade [

	rewriter replace: 'self `;messages; foo: 4; `;messages1'
		with: 'self `;messages1; bar: 4; `;messages'.
	self compare: (rewriter
				executeTree: (self
							parseExpression: 'self foo; printString; foo: 4; bar. self foo: 4');
				tree)
		to: (self
				parseExpression: 'self bar; bar: 4; foo; printString. self foo:4')
]

{ #category : #'tests - ok' }
RBParseTreeRewriterTest >> testRewriteDoesNotReuseOriginalNodes [
	"Due to a bug in RBPatternVariableNode copyInContext method, creating a new astTree from the rewriter reused some nodes
	 of the original tree, this results in two trees sharing the identical nodes. The original AST now contained statement nodes
	that don't refer to the same parent (the method node) and that is wrong."

	| ast search replace |
	ast := self
		parseMethod:
			'foo
self statement1.
self match.'.
	"all statement nodes have the same parent"
	self assert: ast statements first parent equals: ast statements last parent.
	search := '`msg
`@.statements.
`object match.'.
	replace := '`msg
`@.statements.
`object class.'.
	rewriter := RBParseTreeRewriter new.
	rewriter replaceMethod: search with: replace.
	rewriter executeTree: ast.
	"all statement nodes of the original AST still have the same parent"
	self assert: ast statements first parent equals: ast statements last parent
]

{ #category : #'tests - to be refined' }
RBParseTreeRewriterTest >> testRewriteDynamicArray [
	| newSource |
	rewriter := RBParseTreeRewriter new replace: '
		{`@first. `@second. `@third}' with: 'Array with: `@first  with: `@second  with: `@third'.

	newSource := (rewriter executeTree: (self parseRewriteExpression: ' {(1 @ 255).	(Color lightMagenta). 3}'))
		ifTrue: [ rewriter tree formattedCode].
	self assert: newSource equals: 'Array with: 1 @ 255 with: Color lightMagenta with: 3'
]

{ #category : #'tests - to be refined' }
RBParseTreeRewriterTest >> testRewriteMethods [
	"#('source' 'target' 'source pattern' 'target pattern')"
	#(#('arg1: a arg2: b | temp1 temp2 | self stmt1 ifTrue: [^a]. self arg1: a arg2: b' 'arg2: a arg1: b | temp1 temp2 | self stmt1 ifTrue: [^a]. self arg2: b arg2: a' '`arg1: `var1 `arg2: `var2 | `@temps | ``@.stmts. self `arg1: `var1 `arg2: `var2. `@.stmts1' '`arg2: `var1 `arg1: `var2 | `@temps | ``@.stmts. self `arg2: `var2 `arg2: `var1. `@.stmts1') #('arg1: a arg2: b | temp1 temp2 | self stmt1. self arg1: a arg2: b' 'arg1: a arg2: b | temp1 temp2 | [self stmt1] repeat' '`@args: `@vars | `@temps | `@.stmts. self `@args: `@vars' '`@args: `@vars | `@temps | [`@.stmts] repeat') #('+ a | temps | ^self primitiveValue' '- a | temps | ^self primitiveValue' '+ `temp | `@tmps | `@.stmts' '- `temp | `@tmps | `@.stmts') #('a self stmt1. self stmt2' 'a self stmt1. self stmt2' 'b | `@temps | `@.stmts' 'c | `@temps | `@.stmts') #('a <foo: 1 bar: 2>' 'a <bar: 2 foo: 1>' 'a <`sel1: `#arg1 `sel2: `#arg2>' 'a <`sel2: `#arg2 `sel1: `#arg1>') #('a <foo> self foo' 'b <foo> self foo' 'a `@.stmts' 'b `@.stmts'))
		do:
			[:each |
			| rewrite |
			rewrite := RBParseTreeRewriter new.
			rewrite replaceMethod: (each at: 3) with: each last.
			self compare: (self
						parseMethod: (rewrite
								executeTree: (self parseMethod: each first);
								tree) formattedCode)
				to: (self parseMethod: (each at: 2)).
			rewrite := RBParseTreeRewriter new.
			rewrite replaceTree: (self parseRewriteMethod: (each at: 3))
				withTree: (self parseRewriteMethod: each last).
			self compare: (self
						parseMethod: (rewrite
								executeTree: (self parseMethod: each first);
								tree) formattedCode)
				to: (self parseMethod: (each at: 2))]
]

{ #category : #'tests - to be refined' }
RBParseTreeRewriterTest >> testRewrites [
	"#('source' 'target' 'source pattern' 'target pattern')"
	#(
		(	'[:c | |a| a foo1; foo2]'
			'[:c | |a| b foo1; foo2]'
			'a'
			'b' )
		(	'self foo: 1. bar foo1 foo: 2. (self foo: a) foo: (b foo: c)'
			'self bar: 1. bar foo1 bar: 2. (self bar: a) bar: (b bar: c)'
			'``@rcvr foo: ``@arg1'
			'``@rcvr bar: ``@arg1' )
		('3 + 4'	'4 + 4'		'3'		'4' )
		('a := self a'		'b := self a'		'a'		'b' )
		(	'^self at: 1 put: 2'
			'^self put: 1 put: 2'
			'^`@rcvr `at: `@arg1 put: `@arg2'
			'^`@rcvr put: `@arg1 put: `@arg2' )
		('1 + 2 + 3'		'0 + 0 + 0'		'`#literal'		'0' )
		(
			'1 + 2 + 3. 3 foo: 4'
			'3 + (2 + 1). 4 foo: 3'
			'``@rcvr `msg: ``@arg'
			'``@arg `msg: ``@rcvr' )
		(	'self foo: a bar: b. 1 foo: a bar: b'
			'2 foo: a bar: b. 1 foo: a bar: b'
			'self `@msg: `@args'
			'2 `@msg: `@args' )
		(	'a := b. a := c + d'
			'b := a. a := c + d'
			'`var1 := `var2'
			'`var2 := `var1' )
		(	'^self foo value: 1'
			'self return: (self foo value: 1)'
			'^`@anything'
			'self return: `@anything' )
		(	'self first; second. self first; second. self a. self b'
			'2 timesRepeat: [self first; second]. self a. self b'
			'`.Stmt1. `.Stmt1. `@.stmts'
			'2 timesRepeat: [`.Stmt1]. `@.stmts' )
		(	'[:a | self a: 1 c: 2; b]'
			'[:a | self d: 2 e: 1; f. self halt]'
			'`@rcvr `msg1: `@arg1 `msg2: `@arg2; `msg'
			'`@rcvr d: `@arg2 e: `@arg1; f. self halt' )  ) do:
		[:each |
		| rewrite |
		rewrite := RBParseTreeRewriter new.
		rewrite replace: (each at: 3)
			with: each last.
		self compare: (self parseExpression: (rewrite executeTree: (self parseExpression: each first);
				 tree) formattedCode)
			to: (self parseExpression: (each at: 2))]
]

{ #category : #setup }
RBParseTreeRewriterTest >> treeToBeRewritten [
	^ self parserClass
		parseMethod:
			'method: asdf
	<primitive: 1>
	<primitive: 2>
	^asdf + self foo + asdf'
]
