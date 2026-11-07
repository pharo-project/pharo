"
I am the root of the rules checking AST nodes: my class side answers true to `checksNode`. The tools give me each node of each method, so one rule can report several critiques about one method, each one highlighting the offending node.

## Responsibility:

- Check AST nodes rather than methods, classes or packages.
- Anchor my critiques on the checked node, so that the tools select its source interval.

## Collaborators: 

- `ReSmalllintChecker`, `ReCriticEngine class >> #nodeCritiquesOf:` and `ReAbstractRuleTestCase >> #myCritiquesOnMethod:`: iterate the nodes of a method and give them to me.
- `RePatternCodeRule`: my subclass for rules defined by rewrite expressions.

## Public API and Key Messages

Define `basicCheck: aNode` or `check: aNode forCritiquesDo: aBlock`, together with the class-side description (`ruleName`, `groupSelector`, ...) explained in `ReAbstractRule`. Override `shouldCheckMethod:` to skip methods that cannot violate the rule.

Since I check nodes, sending `check:` with a method answers no critique. Run me on a class or a package with `ReSmalllintChecker`:

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

or on one method with `ReCriticEngine nodeCritiquesOf: SmallFloat64 >> #/`.
"
Class {
	#name : 'ReNodeBasedRule',
	#superclass : 'ReAbstractRule',
	#category : 'Renraku-Rules-Rewrite',
	#package : 'Renraku',
	#tag : 'Rules-Rewrite'
}

{ #category : 'testing' }
ReNodeBasedRule class >> checksNode [

	^ true
]

{ #category : 'testing' }
ReNodeBasedRule class >> isAbstract [

	 ^ self = ReNodeBasedRule
]

{ #category : 'testing' }
ReNodeBasedRule class >> isVisible [

	^ self name ~=  #ReNodeBasedRule
]

{ #category : 'helpers' }
ReNodeBasedRule >> anchorFor: aNode [

	^ ReIntervalSourceAnchor
			entity: aNode
			interval: aNode sourceInterval
]
