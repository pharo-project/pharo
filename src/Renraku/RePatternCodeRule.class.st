"
I am an abstract class holding what is common to the rules defined by rewrite expressions: `ReNodeMatchRule`, which reports the code matching a pattern, and `ReNodeRewriteRule`, which also proposes a transformation of it.

## Responsibility:

- Answer `afterCheck: aNode mappings: aDictionary`, the hook used to validate a match (true by default).
- Flatten the wildcard mappings so that they can be accessed both with the pattern nodes and with their pattern strings, for example `mappings at: '`#floatLiteral'`.

## Collaborators: 

- `OCParser class >> #parseRewriteExpression:`: parses the rewrite expressions into pattern nodes, which match the checked nodes (`match:onSuccess:onFailure:`).
- `ReNodeMatchRule` and `ReNodeRewriteRule`: my concrete subclasses. See their comments to define and run such a rule.
"
Class {
	#name : 'RePatternCodeRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'Renraku-Rules-Rewrite',
	#package : 'Renraku',
	#tag : 'Rules-Rewrite'
}

{ #category : 'testing' }
RePatternCodeRule class >> isAbstract [

	 ^ self = RePatternCodeRule
]

{ #category : 'testing' }
RePatternCodeRule class >> isVisible [

	^ self name ~=  #RePatternCodeRule
]

{ #category : 'testing' }
RePatternCodeRule class >> noteCompilationOf: aSelector meta: isMeta [

	 aSelector == #initialize & isMeta not ifTrue: [
		ReRuleManager reset ]
]

{ #category : 'hooks' }
RePatternCodeRule >> afterCheck: aNode mappings: mappingDict [
	"after a node was match you can do a post-validation here on the node and the dictionalry of wildcars with matched nodes as values"

	^ true
]

{ #category : 'private' }
RePatternCodeRule >> flattenMatchMap: aDictionary [
	^ aDictionary keysAndValuesDo: [ :key :val |
		((key isKindOf: OCProgramNode) and: [
		 key isPatternNode ])
			ifTrue: [
				aDictionary
					at: key pattern
					put: val ] ]
]
