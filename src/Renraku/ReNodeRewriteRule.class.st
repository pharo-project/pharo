"
I am the base rule for code match and rewrite rules: I report the AST nodes matching rewrite expressions and my critiques propose a change rewriting them. The Quality Assistant offers to apply it automatically.

For example, `ReAsOrderedCollectionNotNeededRule` reports

```
aCollection addAll: anArray asOrderedCollection
```

and proposes to rewrite it as `aCollection addAll: anArray`, since `addAll:` accepts any collection.

## Responsibility:

- Hold the pairs (expression to match, rewrite) defined in my `initialize` method.
- Report a `ReReplaceNodeCritique`, which provides the change, for each matching node accepted by `afterCheck:mappings:`.

## Collaborators: 

- `OCParser`: parses my rewrite expressions into pattern nodes, which match and rewrite the checked nodes.
- `ReSmalllintChecker` and `ReCriticEngine`: give me the nodes of the methods to check (on a copy of the AST).

## Public API and Key Messages

Use the following methods in `initialize` to set up a subclass:

- `replace:with:` (or `addMatchingExpression:rewriteTo:`): a rewrite expression to match and the rewrite expression replacing it.
- `replaceMethod:with:` (or `addMatchingMethod:rewriteTo:`): same, parsed as methods.
- `replace:byEvaluating:` (or `addMatchingExpression:rewriteWith:`): a rewrite expression to match and a block answering the node replacing the matched one. The block may accept 2 arguments: the matched node and the dictionary of wildcard mappings.
- `addMatchingMethod:rewriteWith:`: same, parsed as a method.

Override `afterCheck: aNode mappings: aDictionary` to validate a match.

```
initialize
	super initialize.
	self
		replace: '``@receiver addAll: ``@arg asOrderedCollection'
		with: '``@receiver addAll: ``@arg'
```

Run me on a class or a package with `ReSmalllintChecker`:

```
| rule checker |
rule := ReAsOrderedCollectionNotNeededRule new.
checker := ReSmalllintChecker new
	rule: { rule };
	environment: (PackageOrganizer default packageNamed: 'Kernel') asEnvironment;
	yourself.
checker run.
checker criticsOf: rule
```

Replace the environment by `Point asEnvironment` to check one class.

## Internal Representation

- `matches`: a dictionary mapping the pattern nodes to match to their rewrites (pattern nodes or block wrappers).
"
Class {
	#name : 'ReNodeRewriteRule',
	#superclass : 'RePatternCodeRule',
	#instVars : [
		'matches'
	],
	#category : 'Renraku-Rules-Rewrite',
	#package : 'Renraku',
	#tag : 'Rules-Rewrite'
}

{ #category : 'testing' }
ReNodeRewriteRule class >> isAbstract [

	^self == ReNodeRewriteRule
]

{ #category : 'testing' }
ReNodeRewriteRule class >> isVisible [

	^ self name ~=  #ReNodeRewriteRule
]

{ #category : 'adding' }
ReNodeRewriteRule >> addMatchingExpression: aString rewriteTo: aRewriteString [
	"add a 'from->to' pair of strings that represent a rewrite expression string to match and a rewrite expression to replace the matched node."

	matches
		at: (OCParser parseRewriteExpression: aString)
		put: (ReRewriteTreeWrapper on: (
			OCParser parseRewriteExpression: aRewriteString))
]

{ #category : 'adding' }
ReNodeRewriteRule >> addMatchingExpression: aString rewriteWith: aBlock [
	"add 	a 'from->to' pair, first element of which is a rewrite expression in a form of a string that is used to match nodes. The second parameter is a block that has to return a node which should replace the matched one. The block may accept 2 atguments: the matched node, and a dictionary of wildcard variables mapping"

	matches
		at: (OCParser parseRewriteExpression: aString)
		put: (ReRewriteBlockWrapper on: aBlock)
]

{ #category : 'adding' }
ReNodeRewriteRule >> addMatchingMethod: aString rewriteTo: aRewriteString [
	"add a 'from->to' pair of strings that represent a match method string to match and a rewrite method to replace the matched mathod"

	matches
		at: (OCParser parseRewriteMethod: aString)
		put: (ReRewriteTreeWrapper on: (
			OCParser parseRewriteMethod: aRewriteString))
]

{ #category : 'adding' }
ReNodeRewriteRule >> addMatchingMethod: aString rewriteWith: aBlock [
	"add 	a 'from->to' pair, first element of which is a rewrite method code in a form of a string that is used to match methods. The second parameter is a block that has to return a method ast which should replace the matched one. The block may accept 2 atguments: the matched ast, and a dictionary of wildcard variables mapping"

	matches
		at: (OCParser parseRewriteMethod: aString)
		put: (ReRewriteBlockWrapper on: aBlock)
]

{ #category : 'running' }
ReNodeRewriteRule >> check: aNode forCritiquesDo: aCriticBlock [

	matches keysAndValuesDo: [ :matcher :rewriter |
		matcher
			match: aNode
			onSuccess: [ :map |
				(self afterCheck: aNode mappings: (self flattenMatchMap: map))
					ifTrue: [
						aCriticBlock cull:
							(self critiqueFor: aNode by: rewriter withMappings: map) ] ]
			onFailure: [  ] ]
]

{ #category : 'helpers' }
ReNodeRewriteRule >> critiqueFor: aNode by: rewriter withMappings: map [


	^ ReReplaceNodeCritique
		rule: self
		achor: (self anchorFor: aNode)
		oldNode: aNode
		newNode: (rewriter rewriten: aNode with: map)
]

{ #category : 'initialization' }
ReNodeRewriteRule >> initialize [
	super initialize.
	matches := Dictionary new
]

{ #category : 'testing' }
ReNodeRewriteRule >> isRewriteRule [
	^true
]

{ #category : 'adding' }
ReNodeRewriteRule >> replace: aString byEvaluating: aBlock [
	"add 	a 'from->to' pair, first element of which is a rewrite expression in a form of a string that is used to match nodes. The second parameter is a block that has to return a node which should replace the matched one. The block may accept 2 atguments: the matched node, and a dictionary of wildcard variables mapping"

	self addMatchingExpression: aString rewriteWith: aBlock
]

{ #category : 'adding' }
ReNodeRewriteRule >> replace: aString with: aRewriteString [
	"add a 'from->to' pair of strings that represent a match expression string to match and a rewrite expression to replace the matched node"

	self addMatchingExpression: aString rewriteTo: aRewriteString
]

{ #category : 'adding' }
ReNodeRewriteRule >> replaceMethod: aString with: aRewriteString [
	"add a 'from->to' pair of strings that represent a match method string to match and a rewrite method to replace the matched code"

	self addMatchingMethod: aString rewriteTo: aRewriteString
]
