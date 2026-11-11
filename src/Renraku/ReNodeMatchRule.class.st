"
I am the base rule for code pattern matching: I report the AST nodes matching rewrite expressions (patterns with wildcards such as `` `@expr ``, `` `#literal `` or `` `var ``).

For example, `ReFloatEqualityComparisonRule` reports

```
x = 0.0
```

which should be written

```
x isZero
```

## Responsibility:

- Hold the rewrite expressions to match, defined in my `initialize` method.
- Report a critique for each matching node accepted by `afterCheck:mappings:`.

## Collaborators: 

- `OCParser`: parses my rewrite expressions into pattern nodes, which match the checked nodes.
- `ReSmalllintChecker` and `ReCriticEngine`: give me the nodes of the methods to check.

## Public API and Key Messages

Use the following methods in `initialize` to set up a subclass:

- `matches:` (or `addMatchingExpression:`) adds a rewrite expression to match.
- `matchesAny:` same as the previous one but takes a collection of strings.
- `addMatchingMethod:` adds a rewrite expression parsed as a method.

Override `afterCheck: aNode mappings: aDictionary` to validate a matched node with the bindings of its wildcards.

```
ReFloatEqualityComparisonRule >> initialize
	super initialize.
	self matchesAny: #(
		'`#floatLiteral = `@expr'
		'`#floatLiteral ~= `@expr'
		'`@expr = `#floatLiteral'
		'`@expr ~= `#floatLiteral' )

ReFloatEqualityComparisonRule >> afterCheck: aNode mappings: mappingDict
	^ (mappingDict at: '`#floatLiteral') value isFloat
```

To run a match rule on a class or a package, use `ReSmalllintChecker`:

```
| rule checker |
rule := ReFloatEqualityComparisonRule new.
checker := ReSmalllintChecker new
	rule: { rule };
	environment: (PackageOrganizer default packageNamed: 'Kernel') asEnvironment;
	yourself.
checker run.
checker criticsOf: rule
```

Replace the environment by `Point asEnvironment` to check one class.

## Internal Representation

- `matchers`: the pattern nodes parsed from my rewrite expressions.
"
Class {
	#name : 'ReNodeMatchRule',
	#superclass : 'RePatternCodeRule',
	#instVars : [
		'matchers'
	],
	#category : 'Renraku-Rules-Rewrite',
	#package : 'Renraku',
	#tag : 'Rules-Rewrite'
}

{ #category : 'testing' }
ReNodeMatchRule class >> isAbstract [

	 ^ self = ReNodeMatchRule
]

{ #category : 'testing' }
ReNodeMatchRule class >> isVisible [

	^ self name ~= #ReNodeMatchRule
]

{ #category : 'adding' }
ReNodeMatchRule >> addMatchingExpression: aString [
	"add a string of rewrite expression to be matched by rule"

	matchers add: (OCParser parseRewriteExpression: aString)
]

{ #category : 'adding' }
ReNodeMatchRule >> addMatchingMethod: aString [
	"add a string of rewrite expression which should be parsed as a method"

	matchers add: (OCParser parseRewriteMethod: aString)
]

{ #category : 'running' }
ReNodeMatchRule >> check: aNode forCritiquesDo: aCriticBlock [

	matchers do: [ :matcher |
		matcher
			match: aNode
			onSuccess: [ :map |
				(self
					afterCheck: aNode
					mappings: (self flattenMatchMap: map))
						ifTrue: [
							aCriticBlock cull: (self critiqueFor: aNode) ] ]
			onFailure: [  ] ]
]

{ #category : 'initialization' }
ReNodeMatchRule >> initialize [
	super initialize.
	matchers := Set new
]

{ #category : 'adding' }
ReNodeMatchRule >> matches: aString [
	"add a string of rewrite expression to be matched by rule"

	self addMatchingExpression: aString
]

{ #category : 'adding' }
ReNodeMatchRule >> matchesAny: aStringCollection [
	"add a collection of rewrite expression strings to be matched by the rule"

	aStringCollection do: [ :string |
		self addMatchingExpression: string ]
]
