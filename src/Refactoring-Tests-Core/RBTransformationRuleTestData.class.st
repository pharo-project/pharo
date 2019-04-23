Class {
	#name : #RBTransformationRuleTestData,
	#superclass : #RBFooLintRuleTestData,
	#instVars : [
		'rewriteRule',
		'builder',
		'class'
	],
	#classVars : [
		'RecursiveSelfRule'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #transformations }
RBTransformationRuleTestData class >> assignmentInIfTrue [
	^self rewrite: #(
			#('``@Boolean ifTrue: [`variable := ``@true] ifFalse: [`variable := ``@false]'
			"->"
			'`variable := ``@Boolean ifTrue: [``@true] ifFalse: [``@false]')
			#('``@Boolean ifFalse: [`variable := ``@true] ifTrue: [`variable := ``@false]'
			"->"
			'`variable := ``@Boolean ifFalse: [``@true] ifTrue: [``@false]'))
		methods: false
		name: 'Move variable assignment outside of single statement ifTrue:ifFalse: blocks'
]

{ #category : #transformations }
RBTransformationRuleTestData class >> atIfAbsent [
	^self rewrite: #(
			#('``@dictionary at: ``@key 
					ifAbsent: [| `@temps | 
							``@.Statements1.
							``@dictionary at: ``@key put: ``@object.
							``@.Statements2.
							``@object]'
			"->"
			'``@dictionary at: ``@key
					ifAbsentPut: [| `@temps |
							``@.Statements1.
							``@.Statements2.
							``@object]')
			#('``@dictionary at: ``@key
					ifAbsent: [| `@temps |
							``@.Statements.
							``@dictionary at: ``@key put: ``@object]'
			"->"
			'``@dictionary at: ``@key
					ifAbsentPut: [| `@temps |
							``@.Statements.
							``@object]'))
		methods: false
		name: 'at:ifAbsent: -> at:ifAbsentPut:'
]

{ #category : #transformations }
RBTransformationRuleTestData class >> betweenAnd [
	^self rewrite: #(
			#('``@a >= ``@b and: [``@a <= ``@c]' "->" '``@a between: ``@b and: ``@c')
			#('``@a >= ``@b & (``@a <= ``@c)' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a and: [``@a <= ``@c]' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a & (``@a <= ``@c)' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c and: [``@a >= ``@b]' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c & (``@a >= ``@b)' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a and: [``@a >= ``@b]' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a & (``@a >= ``@b)' "->" '``@a between: ``@b and: ``@c')
			#('``@a >= ``@b and: [``@c >= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@a >= ``@b & (``@c >= ``@a)' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a and: [``@c >= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a & (``@c >= ``@a)' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c and: [``@b <= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c & (``@b <= ``@a)' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a and: [``@b <= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a & (``@b <= ``@a)' "->" '``@a between: ``@b and: ``@c'))
		methods: false
		name: '"a >= b and: [a <= c]" -> "a between: b and: c"'
]

{ #category : #cleanup }
RBTransformationRuleTestData class >> cleanUp [
	self nuke
]

{ #category : #transformations }
RBTransformationRuleTestData class >> detectIfNone [
	^self rewrite: #(
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) isNil'
				"->"	'(``@collection contains: [:`each | | `@temps | ``@.Statements]) not')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) = nil'
				"->"	'(``@collection contains: [:`each | | `@temps | ``@.Statements]) not')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) == nil'
				"->"	'(``@collection contains: [:`each | | `@temps | ``@.Statements]) not')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) notNil'
				"->"	'``@collection contains: [:`each | | `@temps | ``@.Statements]')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~= nil'
				"->"	'``@collection contains: [:`each | | `@temps | ``@.Statements]')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~~ nil'
				"->"	'``@collection contains: [:`each | | `@temps | ``@.Statements]'))
		methods: false
		name: 'detect:ifNone: -> contains:'
]

{ #category : #transformations }
RBTransformationRuleTestData class >> equalNil [
	^self
		rewrite: #(
			#('``@object = nil'	"->"	'``@object isNil') 
			#('``@object == nil'	"->"	'``@object isNil') 
			#('``@object ~= nil'	"->"	'``@object notNil') 
			#('``@object ~~ nil'	"->"	'``@object notNil'))
		methods: false
		name: '= nil -> isNil AND ~= nil -> notNil'
]

{ #category : #transformations }
RBTransformationRuleTestData class >> guardClause [
	^self
		rewrite: #(
			#('`@methodName: `@args 
					| `@temps | 
					`@.Statements. 
					`@condition ifTrue: [| `@trueTemps | `.Statement1. `.Statement2. `@.Statements1]'
			"->"
			'`@methodName: `@args
					| `@temps `@trueTemps |
					`@.Statements.
					`@condition ifFalse: [^self].
					`.Statement1.
					`.Statement2.
					`@.Statements1') 
			#('`@methodName: `@args 
					| `@temps | 
					`@.Statements. 
					`@condition ifFalse: [| `@falseTemps | `.Statement1. `.Statement2. `@.Statements1]'
			"->"
			'`@methodName: `@args
					| `@temps `@falseTemps |
					`@.Statements.
					`@condition ifTrue: [^self].
					`.Statement1.
					`.Statement2.
					`@.Statements1'))
		methods: true
		name: 'Eliminate guarding clauses'
]

{ #category : #'class initialization' }
RBTransformationRuleTestData class >> initializeAfterLoad1 [
	RecursiveSelfRule := RBParseTreeSearcher new.
	RecursiveSelfRule
		addMethodSearches: #('`@methodName: `@args | `@temps | self `@methodName: `@args' '`@methodName: `@args | `@temps | ^self `@methodName: `@args')
				-> [:aNode :answer | true]
]

{ #category : #transformations }
RBTransformationRuleTestData class >> minMax [
	^self rewrite: #(
			#('``@a < ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a min: ``@b')
			#('``@a <= ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a min: ``@b')
			#('``@a > ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a max: ``@b')
			#('``@a >= ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a max: ``@b')
			#('``@a < ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a max: ``@b')
			#('``@a <= ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a max: ``@b')
			#('``@a > ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a min: ``@b')
			#('``@a >= ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a min: ``@b')
			#('`a < ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('`a <= ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('`a < ``@b ifFalse: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('`a <= ``@b ifFalse: [`a := ``@b]'			"->"	'`a := `a min: ``@b')
			#('`a > ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('`a >= ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('`a > ``@b ifFalse: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('`a >= ``@b ifFalse: [`a := ``@b]'			"->"	'`a := `a max: ``@b')
			#('``@b < `a ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('``@b <= `a ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('``@b < `a ifFalse: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('``@b <= `a ifFalse: [`a := ``@b]'			"->"	'`a := `a max: ``@b')
			#('``@b > `a ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('``@b >= `a ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('``@b > `a ifFalse: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('``@b >= `a ifFalse: [`a := ``@b]'			"->"	'`a := `a min: ``@b'))
		methods: false
		name: 'Rewrite ifTrue:ifFalse: using min:/max:'
]

{ #category : #transformations }
RBTransformationRuleTestData class >> notElimination [
	^self
		rewrite: #(
			#('``@object not not'	"->"	'``@object') 
			#('``@object not ifTrue: ``@block' 	"->"	'``@object ifFalse: ``@block') 
			#('``@object not ifFalse: ``@block'	"->"	'``@object ifTrue: ``@block') 
			#('``@collection select: [:`each | | `@temps | ``@.Statements. ``@object not]'
				"->"	'``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object]')
			#('``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object not]'
				"->"	'``@collection select: [:`each | | `@temps | ``@.Statements. ``@object]')
			#('[| `@temps | ``@.Statements. ``@object not] whileTrue: ``@block'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileFalse: ``@block')
			#('[| `@temps | ``@.Statements. ``@object not] whileFalse: ``@block'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileTrue: ``@block')
			#('[| `@temps | ``@.Statements. ``@object not] whileTrue'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileFalse')
			#('[| `@temps | ``@.Statements. ``@object not] whileFalse'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileTrue')
			#('(``@a <= ``@b) not' "->" '``@a > ``@b')
			#('(``@a < ``@b) not' "->" '``@a >= ``@b')
			#('(``@a = ``@b) not' "->" '``@a ~= ``@b')
			#('(``@a == ``@b) not' "->" '``@a ~~ ``@b')
			#('(``@a ~= ``@b) not' "->" '``@a = ``@b')
			#('(``@a ~~ ``@b) not' "->" '``@a == ``@b')
			#('(``@a >= ``@b) not' "->" '``@a < ``@b')
			#('(``@a > ``@b) not' "->" '``@a <= ``@b'))			
		methods: false
		name: 'Eliminate unnecessary not''s'
]

{ #category : #'class initialization' }
RBTransformationRuleTestData class >> nuke [
	RecursiveSelfRule := nil
]

{ #category : #'instance creation' }
RBTransformationRuleTestData class >> rewrite: stringArrays methods: aBoolean name: aName [ 
	| rewriteRule |
	rewriteRule := RBParseTreeRewriter new.
	stringArrays do: 
			[:each | 
			aBoolean
				ifTrue: [rewriteRule addMethodSearch: each first -> each last]
				ifFalse: [rewriteRule addSearch: each first -> each last]].
	^(self new) name: aName;
		rewriteUsing: rewriteRule;
		yourself
]

{ #category : #transformations }
RBTransformationRuleTestData class >> showWhileBlocks [
	^self
		rewrite: #(
			#('``@cursor showWhile: [| `@temps | ``@.Statements. `var := ``@object]'
				"->"	'`var := ``@cursor showWhile: [| `@temps | ``@.Statements. ``@object]') 
			#('``@cursor showWhile: [| `@temps | ``@.Statements. ^``@object]'
				"->"	'^``@cursor showWhile: [| `@temps | ``@.Statements. ``@object]'))
		methods: false
		name: 'Move assignment out of showWhile: blocks'
]

{ #category : #transformations }
RBTransformationRuleTestData class >> superSends [
	^(self new)
		name: 'Rewrite super messages to self messages when both refer to same method';
		superSends;
		yourself
]

{ #category : #transformations }
RBTransformationRuleTestData class >> unwindBlocks [
	^self
		rewrite: #(
			#('[| `@temps | ``@.Statements. `var := ``@object] ensure: ``@block'
				"->"	'`var := [| `@temps | ``@.Statements. ``@object] ensure: ``@block') 
			#('[| `@temps | ``@.Statements. ^``@object] ensure: ``@block'
				"->"	'^[| `@temps | ``@.Statements. ``@object] ensure: ``@block') 
			#('[| `@temps | ``@.Statements. `var := ``@object] ifCurtailed: ``@block'
				"->"	'`var := [| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block') 
			#('[| `@temps | ``@.Statements. ^``@object] ifCurtailed: ``@block'
				"->"	'^[| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block'))
		methods: false
		name: 'Move assignment out of unwind blocks'
]

{ #category : #accessing }
RBTransformationRuleTestData >> checkMethod: aSmalllintContext [ 
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
				ifFalse: 
					[builder compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]]
]

{ #category : #testing }
RBTransformationRuleTestData >> hasConflicts [
	^true
]

{ #category : #testing }
RBTransformationRuleTestData >> isEmpty [
	^builder changes isEmpty
]

{ #category : #accessing }
RBTransformationRuleTestData >> problemCount [
	^builder problemCount
]

{ #category : #accessing }
RBTransformationRuleTestData >> resetResult [
	builder := RBRefactoryChangeManager changeFactory compositeRefactoryChange
]

{ #category : #initialization }
RBTransformationRuleTestData >> rewriteUsing: searchReplacer [ 
	rewriteRule := searchReplacer.
	self resetResult
]

{ #category : #rules }
RBTransformationRuleTestData >> superSends [
	| rule |
	rule := RBParseTreeRewriter new.
	rule addSearch: 'super `@message: ``@args'
				-> (
					[:aNode | 
					(class withAllSubclasses
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil]
							-> 'self `@message: ``@args').
	self rewriteUsing: rule
]

{ #category : #private }
RBTransformationRuleTestData >> viewResults [
	"I reset the result so that we don't fill up memory with methods to compile in the builder."

	builder inspect.
	self resetResult
]
