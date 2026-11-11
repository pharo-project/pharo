"
I am the root of all the quality rules of Renraku. A rule describes a constraint on code entities (methods, classes or traits, packages and AST nodes): it checks an entity and reports a critique for each violation it finds.

## Responsibility:

- Declare, on the class side, which kinds of entities I check (`checksMethod`, `checksClass`, `checksPackage`, `checksNode`) and how the tools present me (`ruleName`, `rationale`, `groupSelector`, `severity`).
- Check an entity with `basicCheck:` or `check:forCritiquesDo:` and build the critiques of the violations.
- Let the tools find me: every subclass interested in at least one kind of entity is a visible rule (`isVisible`).

## Collaborators: 

- `ReRuleManager`: caches the visible rules and gives them to the tools such as the Quality Assistant.
- `ReAbstractCritique` and `ReTrivialCritique`: the critiques I report, pointing to the entity through a `ReSourceAnchor`.
- `ReSmalllintChecker` and `ReCriticEngine`: run rules on packages, classes and methods.

## Public API and Key Messages

To define a rule, subclass me (or `ReNodeMatchRule`, `ReNodeRewriteRule` for rules on code patterns) and define:

- class side `checksMethod`, `checksClass`, `checksPackage` or `checksNode` answering true.
- class side `ruleName`: a short name, used as the title of the critiques.
- class side `rationale`: a one-line explanation. It answers my class comment by default.
- class side `groupSelector`: one of my class-side `…Group` selectors, for example `#potentialBugsGroup`. Default: `#unclassifiedMethodsGroup`.
- class side `severity`: `'information'`, `'warning'` (default) or `'error'`.
- `basicCheck: anEntity` answering true when the entity violates the rule, or `check: anEntity forCritiquesDo: aBlock` to report several critiques (build them with `critiqueFor:` or `critiqueFor:about:`).

For example:

```
ReNoClassCommentRule class >> checksClass
	^ true

ReNoClassCommentRule class >> ruleName
	^ 'No class comment'

ReNoClassCommentRule class >> groupSelector
	^ #codingIdiomViolationGroup

ReNoClassCommentRule >> basicCheck: aClass
	(aClass isMeta or: [ aClass isTestCase ]) ifTrue: [ ^ false ].
	^ aClass hasComment not
```

The class comment of a rule should describe it, show a short violating code example together with the corrected version, and show how to execute the rule. A rule needs no class-side `example…` or `script…` methods: the examples belong to its comment. Test a rule in a subclass of `ReAbstractRuleTestCase` named after the rule followed by `Test`.

### Executing a rule

`check:` answers the critiques of one entity:

```
ReExcessiveMethodsRule new check: Object.
ReNoClassCommentRule new check: Point
```

A rule checking AST nodes reports nothing when it receives a method. `ReSmalllintChecker` dispatches the package, its classes, their methods and the AST nodes of the methods to the rule, so it runs any rule on a class or on a package:

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

Use `(PackageOrganizer default packageNamed: 'Kernel') asEnvironment` as the environment to check a whole package.

### Reset the cache

The rule cache of `ReRuleManager` is reset when a rule class is added or removed. Once a new rule is complete (for example after defining `checksMethod`), execute `ReRuleManager reset` so that the tools take it into account.

## Internal Representation

I have no state: my description is defined on my class side and my instance-side `ruleName`, `rationale`, `group` and `severity` delegate to it.

### Implementation Points

- `uniqueIdentifierName` identifies the rule in the manifests storing the banned critiques. It should change only when the rule completely changes its semantics.
- `shouldCheckMethod:` lets a node rule skip methods that cannot violate it.
"
Class {
	#name : 'ReAbstractRule',
	#superclass : 'Object',
	#classInstVars : [
		'enabled'
	],
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing - rule group' }
ReAbstractRule class >> apiChangeGroup [

	^ 'API Change'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> apiHintsGroup [

	^ 'API Hints'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> architecturalGroup [

	^ 'Architectural'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> bugsGroup [

	^ 'Bugs'
]

{ #category : 'testing - interest' }
ReAbstractRule class >> checksClass [
	"Not checking a class by default - but subclasses should return true when the receiver rule checks on class level."

	^ false
]

{ #category : 'testing - interest' }
ReAbstractRule class >> checksMethod [
	"Not checking a method by default - but subclasses should return true when the receiver rule checks on method level."

	^ false
]

{ #category : 'testing - interest' }
ReAbstractRule class >> checksNode [
	"Not checking a node by default - but subclasses should return true when the receiver rule checks on AST node level."

	^ false
]

{ #category : 'testing - interest' }
ReAbstractRule class >> checksPackage [
	"Not checking a package by default - but subclasses should return true when the receiver rule checks on package level."

	^ false
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> cleanCodeGroup [

	^ 'Clean Code'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> codingIdiomViolationGroup [

	^ 'Coding Idiom Violation'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> designFlawGroup [

	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReAbstractRule class >> enabled [

	^ enabled ifNil: [ enabled := self enabledByDefault ]
]

{ #category : 'accessing' }
ReAbstractRule class >> enabled: aBoolean [

	enabled := aBoolean.
	ReRuleManager reset
]

{ #category : 'manifest' }
ReAbstractRule class >> enabledByDefault [

	^ true
]

{ #category : 'accessing' }
ReAbstractRule class >> enabledSettingID [

	^ (self name, '_enabled') asSymbol
]

{ #category : 'accessing' }
ReAbstractRule class >> group [

	^ self perform: self groupSelector
]

{ #category : 'accessing' }
ReAbstractRule class >> groupSelector [

	^  #unclassifiedMethodsGroup
]

{ #category : 'manifest' }
ReAbstractRule class >> identifierMinorVersionNumber [
	"This number identifies the version of the rule definition. Each time the rule is updated and its changes invalidates previous false positives identification (and as such should be reassessed by developers) the number should be increased."

	^ 1
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> instanceMethodProtocolGroup [

	^ 'Instance Method Protocol'
]

{ #category : 'testing' }
ReAbstractRule class >> isAbstract [

	^ self = ReAbstractRule
]

{ #category : 'manifest' }
ReAbstractRule class >> isBeginnerRule [

	^ false
]

{ #category : 'manifest' }
ReAbstractRule class >> isDefaultRule [

	^ true
]

{ #category : 'testing' }
ReAbstractRule class >> isUsed [
	"all my sublasses are used"

	^self name = 'ReAbstractRule'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : 'testing' }
ReAbstractRule class >> isVisible [

	self isAbstract ifTrue: [ ^ false ].
	
	^ self methodOfInteresetSelectors anySatisfy: [ :selector |
		self perform: selector ]
]

{ #category : 'properties' }
ReAbstractRule class >> methodOfInteresetSelectors [

	^ #(checksMethod checksClass checksPackage checksNode)
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> methodProtocolNameGroup [

	^ 'Method Protocol Name'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> optimizationGroup [

	^ 'Optimization'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> potentialBugsGroup [

	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReAbstractRule class >> rationale [
	"Answer an explanation of the rule, usually in one line. Long description can be obtained using longDescription."

	^ self comment
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> rubricGroup [

	^ 'Rubric'
]

{ #category : 'accessing' }
ReAbstractRule class >> ruleName [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReAbstractRule class >> severity [
	"Answer the severity of issues reported by this rule. This method should return one of 'error', 'warning', or 'information'."

	^ 'warning'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> styleGroup [

	^ 'Style'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> sunitGroup [

	^ 'SUnit'
]

{ #category : 'accessing - rule group' }
ReAbstractRule class >> unclassifiedMethodsGroup [

	^ 'Unclassified rules'
]

{ #category : 'manifest' }
ReAbstractRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ self name
]

{ #category : 'manifest' }
ReAbstractRule class >> uniqueIdentifierNumber [
	"This number should be unique and should change only when the rule completely change semantics"
	^ 0
]

{ #category : 'helpers' }
ReAbstractRule >> anchorFor: anEntity [

	^ ReSourceAnchor entity: anEntity
]

{ #category : 'running' }
ReAbstractRule >> basicCheck: anEntity [
	"If you end here, you should most likely implement this in a subclass"

	^ self shouldBeImplemented
]

{ #category : 'running' }
ReAbstractRule >> check: anEntity [

	| critiques |
	critiques := OrderedCollection new.
	self
		check: anEntity
		forCritiquesDo: [ :critique | critiques add: critique ].
	^ critiques
]

{ #category : 'running' }
ReAbstractRule >> check: anEntity forCritiquesDo: aCriticBlock [
	"Accepts an entity and a block which could be evaluated for each detected critique
	aCriticBlock may accept one argument: the critique object"

	(self basicCheck: anEntity)
		ifTrue: [ aCriticBlock cull: (self critiqueFor: anEntity) ]
]

{ #category : 'running' }
ReAbstractRule >> check: anEntity forCritiquesDo: aCriticBlock ifNone: alternativeBlock [

	| encounteredCritique |
	encounteredCritique := false.

	self check: anEntity forCritiquesDo: [ :crit |
		encounteredCritique := true.
		aCriticBlock cull: crit ].

	encounteredCritique ifFalse: alternativeBlock
]

{ #category : 'running' }
ReAbstractRule >> check: anEntity ifNoCritiques: alternativeBlock [

	| critiques |
	critiques := OrderedCollection new.
	self
		check: anEntity
		forCritiquesDo: [ :critique | critiques add: critique ]
		ifNone: alternativeBlock.
	^ critiques
]

{ #category : 'utilities' }
ReAbstractRule >> createTrivialCritiqueOn: method intervalOf: node hint: hint [

	^ ReTrivialCritique
		  withAnchor:
		  (ReIntervalSourceAnchor
			   entity: method
			   interval: node sourceInterval)
		  by: self
		  hint: hint
]

{ #category : 'helpers' }
ReAbstractRule >> critiqueFor: anEntity [

	^ ReTrivialCritique
		withAnchor: (self anchorFor: anEntity)
		by: self
]

{ #category : 'running - helpers' }
ReAbstractRule >> critiqueFor: aClass about: aVarName [

	| crit |
	crit := ReTrivialCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aVarName)
		by: self.
	crit tinyHint: aVarName.
	^ crit
]

{ #category : 'accessing' }
ReAbstractRule >> group [
	^ self class group
]

{ #category : 'testing' }
ReAbstractRule >> isComposite [

	^ false
]

{ #category : 'testing' }
ReAbstractRule >> isRewriteRule [

	^ false
]

{ #category : 'accessing' }
ReAbstractRule >> name [
	"helper method to support polymorphic API with composite (Group)"
	^ self ruleName
]

{ #category : 'printing' }
ReAbstractRule >> printOn: aStream [

	super printOn: aStream.
	aStream << ' - '; << self severity
]

{ #category : 'accessing' }
ReAbstractRule >> rationale [
	^ self class rationale
]

{ #category : 'compatibility' }
ReAbstractRule >> resetResult [
]

{ #category : 'accessing' }
ReAbstractRule >> ruleName [

	^ self class ruleName
]

{ #category : 'accessing' }
ReAbstractRule >> severity [
	"Answer the severity of issues reported by this rule. This method should return one of 'error', 'warning', or 'information'."

	^ self class severity
]

{ #category : 'testing' }
ReAbstractRule >> shouldCheckMethod: aMethod [
	"This is a hook when a rule is checking methods to speed up the rule and skip some methods for the nodes analysis."

	^ true
]
