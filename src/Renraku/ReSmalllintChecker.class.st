"
I run a set of Renraku rules on an environment (a package, a class, ...) and collect their critiques. I give each rule the entities it is interested in: the packages to the package rules, the classes to the class rules, the methods to the method rules and the AST nodes of each method to the node rules.

## Responsibility:

- Split my rules according to the entities they check (`reParseRule`).
- Visit the entities of my environment and collect the critiques of my rules, leaving out the ones banned in the manifests.
- Answer the critiques of one rule (`criticsOf:`).

## Collaborators: 

- `ReAbstractRule` and its subclasses: the rules I run.
- `RBBrowserEnvironment`: the code I check, for example `aPackage asEnvironment` or `aClass asEnvironment`.
- `TheManifestBuilder`: knows the critiques banned or marked as false positives.

## Public API and Key Messages

- `rule:` sets the collection of rules to run, `environment:` the code to check, and `run` runs them.
- `runRules: aCollectionOfRules onPackage: aPackage` sets both and runs.
- `criticsOf: aRule` answers the critiques reported by the rules of the class of `aRule`.

Run one rule on a class:

```
| rule checker |
rule := ReFloatEqualityComparisonRule new.
checker := ReSmalllintChecker new
	rule: { rule };
	environment: Point asEnvironment;
	yourself.
checker run.
checker criticsOf: rule
```

Run the default rules on a package:

```
| checker |
checker := ReSmalllintChecker new.
checker
	runRules: ReRuleManager defaultRules
	onPackage: (PackageOrganizer default packageNamed: 'Kernel').
checker criticsOf: ReFloatEqualityComparisonRule new
```

## Internal Representation

- `methodRules`, `nodeRules`, `classRules`, `packageRules`: my rules, grouped by the entities they check.
- `critiques`: the critiques collected by `run`.
- `manifestClassCache`: the manifest builders of the visited packages.
"
Class {
	#name : 'ReSmalllintChecker',
	#superclass : 'RBSmalllintChecker',
	#instVars : [
		'manifestClassCache',
		'methodRules',
		'nodeRules',
		'classRules',
		'packageRules',
		'critiques'
	],
	#category : 'Renraku-CriticBrowser',
	#package : 'Renraku',
	#tag : 'CriticBrowser'
}

{ #category : 'adding' }
ReSmalllintChecker >> addCritique: aCritique [
		^ critiques add: aCritique
]

{ #category : 'accessing' }
ReSmalllintChecker >> builderManifestClass [
	^ TheManifestBuilder
]

{ #category : 'private' }
ReSmalllintChecker >> checkClass: aClass [

	(environment includesClass: aClass) ifFalse: [ ^ self ].

	self getCritiquesAbout: aClass by: classRules
]

{ #category : 'private' }
ReSmalllintChecker >> checkMethodsForClass: aClass [

	environment methodsForClass: aClass do: [ :method |
		self getCritiquesAbout: method by: methodRules.
		nodeRules do: [ :r |
			(r shouldCheckMethod: method) ifTrue: [
				| ast |
				ast := method ast.
				"for rewrite rules, we run every rule on a copy of the ast"
				r isRewriteRule ifTrue: [ ast := ast copy ].
				ast nodesDo: [ :node |
					r check: node forCritiquesDo: [ :crit |
						crit sourceAnchor initializeEnitity: method.
						self addCritique: crit ] ] ] ] ]
]

{ #category : 'private' }
ReSmalllintChecker >> checkPackage: aPackage [

	(environment includesPackage: aPackage) ifTrue: [ self getCritiquesAbout: aPackage by: packageRules ]
]

{ #category : 'accessing' }
ReSmalllintChecker >> classRules [

	^ classRules
]

{ #category : 'accessing' }
ReSmalllintChecker >> classRules: aCollectionOfRules [
	"Select and set class rules from aCollectionOfRules"

	classRules := Set new.
	aCollectionOfRules
		select: [ :r | r class checksClass ]
		thenDo: [ : r | classRules add: r ]
]

{ #category : 'manifest' }
ReSmalllintChecker >> cleanAllManifest [
	| manifests |
	manifests := environment packages collect: [ :cl | self builderManifestClass of: cl ] as: Set.
	manifests do: [ :each | each cleanUp ]
]

{ #category : 'accessing' }
ReSmalllintChecker >> criticsOf: aRule [

	^ critiques
		select: [ :c | c rule class = aRule class ]
]

{ #category : 'manifest' }
ReSmalllintChecker >> falsePositiveOf: aRule [

	| ruleCritiques rId rV|
	ruleCritiques := self criticsOf: aRule.
	rId := aRule class uniqueIdentifierName.
	rV := aRule class identifierMinorVersionNumber.
	^ ruleCritiques select: [ :critique |
		  | entity |
		  entity := critique sourceAnchor entity.
		self isFalsePositive: entity forRuleId: rId versionId: rV]
]

{ #category : 'private' }
ReSmalllintChecker >> getCritiquesAbout: anEntity by: rules [

	rules do: [ :r |
		[ r
			check: anEntity
			forCritiquesDo: [ :crit |
				self addCritique: crit ] ]

			on: Error
			do: [ :er | ReSettings showErrors ifTrue: [ er pass ]  ] ]
]

{ #category : 'initialization' }
ReSmalllintChecker >> initialize [
	super initialize.
	manifestClassCache := Dictionary new.
	self resetResult
]

{ #category : 'manifest' }
ReSmalllintChecker >> isFalsePositive: aCritic forRuleId: ruleId versionId: versionId [

	^ (self manifestBuilderOf: aCritic)
		  ifNil: [ false ]
		  ifNotNil: [ :manifestBuilder | manifestBuilder isFalsePositive: aCritic onRule: ruleId version: versionId ]
]

{ #category : 'manifest' }
ReSmalllintChecker >> isToDo: aCritic forRuleId: ruleId versionId: versionId [

	| mb |

	mb := self manifestBuilderOf: aCritic.
	^ mb ifNil: [ false ] ifNotNil: [ mb containsToDo: aCritic onRule: ruleId version: versionId ]
]

{ #category : 'manifest' }
ReSmalllintChecker >> isTruePositive: aCritic forRuleId: ruleId versionId: versionId [

	^ (self manifestBuilderOf: aCritic)
			containsTruePositive: aCritic onRule:  ruleId version: versionId
]

{ #category : 'accessing' }
ReSmalllintChecker >> manifestBuilderOf: anEntity [

	^ anEntity manifestBuilderForRuleChecker: self
]

{ #category : 'accessing' }
ReSmalllintChecker >> manifestBuilderOfClass: aClass [

	^ self manifestBuilderOfPackage: aClass package
]

{ #category : 'accessing' }
ReSmalllintChecker >> manifestBuilderOfMethod: aMethod [

	^ manifestClassCache at: aMethod methodClass name , aMethod protocolName ifAbsentPut: [
		  (self builderManifestClass hasManifestFor: aMethod)
			  ifTrue: [ self builderManifestClass of: aMethod ]
			  ifFalse: [ nil ] ]
]

{ #category : 'accessing' }
ReSmalllintChecker >> manifestBuilderOfPackage: aPackage [

	^ manifestClassCache
		  at: aPackage name
		  ifAbsentPut: [ self builderManifestClass ofPackage: aPackage ]
]

{ #category : 'accessing' }
ReSmalllintChecker >> packageRules [
	^ packageRules
]

{ #category : 'private' }
ReSmalllintChecker >> reParseRule [

	methodRules := Set new.
	nodeRules := Set new.
	classRules := Set new.
	packageRules := Set new.

	self rule do: [ :r |
		r class checksMethod ifTrue: [ methodRules add: r ].
		r class checksNode ifTrue: [ nodeRules add: r ].
		r class checksClass ifTrue: [ classRules add: r ].
		r class checksPackage ifTrue: [ packageRules add: r ] ]
]

{ #category : 'manifest' }
ReSmalllintChecker >> rejectClassesOf: aPackage [

	| mb |

	mb := self manifestBuilderOf: aPackage.
	^ mb ifNil: [ {} ] ifNotNil: [ mb rejectClasses ]
]

{ #category : 'manifest' }
ReSmalllintChecker >> rejectRulesOf: aPackage [

	| mb |

	mb := self manifestBuilderOf: aPackage.
	^ mb ifNil: [ {} ] ifNotNil: [ mb rejectRules ]
]

{ #category : 'running' }
ReSmalllintChecker >> resetResult [

	critiques := Set new
]

{ #category : 'accessing' }
ReSmalllintChecker >> rule [

	^ (rule isKindOf: RBCompositeLintRule)
		ifTrue: [ rule leaves ]
		ifFalse: [ rule ]
]

{ #category : 'actions' }
ReSmalllintChecker >> run [
	self reParseRule.
	super run
]

{ #category : 'actions' }
ReSmalllintChecker >> runRules: aCompositeRule onPackage: aPackage [

	rule := aCompositeRule.
	environment := RBPackageEnvironment package: aPackage.
	self run
]

{ #category : 'actions' }
ReSmalllintChecker >> runRules: aCompositeRule onPackage: aPackage withoutTestCase: avoidTestCases [
	rule := aCompositeRule.
	environment := RBPackageEnvironment package: aPackage.

	environment := environment withoutClasses: (environment classes select: #isManifest).
	avoidTestCases
		ifTrue: [ environment := environment withoutClasses: (environment classes select: #isTestCase) ].
	self run
]

{ #category : 'manifest' }
ReSmalllintChecker >> toDoOf: aRule [

	| ruleCritiques rId rV mb |

	ruleCritiques := self criticsOf: aRule.
	rId := aRule class uniqueIdentifierName.
	rV := aRule class identifierMinorVersionNumber.
	^ ruleCritiques
		select: [ :critique |
			| entity |

			entity := critique sourceAnchor entity.
			mb := self manifestBuilderOf: entity.
			mb ifNil: [ false ] ifNotNil: [ mb containsToDo: entity onRule: rId version: rV ]
			]
]
