# Renraku Quality Rules

Renraku is a framework for defining and processing quality rules. The framework operates with three main concepts: entities, rules and critiques.

### Entities
Entities are not a part of Renraku, but Renraku is validating entities. Theoretically any object can be an entity, but in practice we mostly focus on code entities such as methods, classes, packages and AST nodes.

### Rules
Rules are the objects that describe constraints about entities. A rule can check an entity and produce critiques that describe the violations of the entity according to the rule.

### Critiques
A critique is an object that binds an entity with a rule that is violated by that entity. The critique describes a specific violation, and may provide a solution to fix it.


## Rules
Rules are constraint descriptions about entities. Think of a rule as a function that consumes an entity and produces a collection of critiques about it (which can be empty if there are no violations).

### Rule Class Hierarchy

- `ReAbstractRule` is the root of all the rules. Subclass it directly for rules that check methods, classes or packages.
- `ReNodeBasedRule` is the root of the rules that check AST nodes (`checksNode` answers true).
- `RePatternCodeRule` holds what is common to the rules based on rewrite expressions.
	- `ReNodeMatchRule` matches code patterns.
	- `ReNodeRewriteRule` matches code patterns and proposes a transformation of the matched code.

Most rules live in the `General-Rules` package. Rules specific to a framework live next to it, for example in `SUnit-Rules`, `Spec2-Rules` or `NewTools-Rules`.

### Creating Rules
Each rule has a few fundamental features to implement. This section walks through all the steps of rule creation. Except `basicCheck:`, `check:forCritiquesDo:` and `initialize`, all these methods are defined on the **class side** of the rule.

Make sure to reset the cache after you completed your rule (see *Reset Cache* below).

#### Subclass
Subclass `ReAbstractRule` or one of the special rule classes presented below. Most of the tools rely on the class hierarchy to select the rules for checking: every subclass of `ReAbstractRule` that is interested in an entity (see below) is a visible rule.

#### Name and Rationale
Each rule should provide a short name string returned from the class-side `ruleName` method. This name is the title of the critiques shown by the tools. The instance-side `name` and `ruleName` methods delegate to the class side.

You should also override the class-side `rationale` method to return a description of the rule, usually in one line. By default `rationale` returns the comment of the rule's class.

#### Specifying an Interest in Entities
The class-side methods `checksMethod`, `checksClass`, `checksPackage` and `checksNode` return true if the rule checks methods, classes or traits, packages and AST nodes respectively. Tools will pass entities of the specified type to the rule for checking. A rule that answers false to all of them is not visible to the tools.

A rule may check multiple types of entities but please avoid checks for types inside of rules. For example, if a rule checks whether an entity is named with a swearing word by obtaining the name of the entity and matching substrings, it is completely fine to specify that the rule checks classes and packages, as you don't have to distinguish which entity was passed to you.

#### Checking
The easiest way to check an entity is to override the `basicCheck:` method. The method accepts an entity as a parameter and returns true if there is a violation and false otherwise.

```st
ReNoClassCommentRule >> basicCheck: aClass

	(aClass isMeta or: [ aClass isTestCase ]) ifTrue: [ ^ false ].
	^ aClass hasComment not
```

#### Advanced Checking
While there is a default implementation which relies on `basicCheck:` and creates an instance of `ReTrivialCritique`, you can override the `check:forCritiquesDo:` method. This method accepts an entity and a block which should be evaluated for each detected critique. This means that one rule can detect multiple critiques about one entity. For example if a rule checks for unused variables it can report all of them with a dedicated critique for each.

The block which should be evaluated for each critique may accept one argument: the critique object, this is why you have to evaluate it with `cull:`. You may use the `critiqueFor:` method if you don't feel comfortable with critiques yet. For example:

```st
self critiqueFor: anEntity
```

will return a `ReTrivialCritique` about the entity. `critiqueFor:about:` builds a critique that highlights a given variable name in the source of the entity. Later you can update your code to create other kinds of critiques more suitable for your case.

Rules checking AST nodes are evaluated on each node of each method. To skip methods that cannot violate the rule, and speed up the analysis, override the `shouldCheckMethod:` hook.

#### Group and Severity
It's a good idea to assign your rule to a specific group. For this override the class-side `groupSelector` method and return the selector of one of the group methods defined on the class side of `ReAbstractRule`. The class-side `group` method answers the corresponding group name.

| `groupSelector` | Group name |
| --- | --- |
| `#apiChangeGroup` | API Change |
| `#apiHintsGroup` | API Hints |
| `#architecturalGroup` | Architectural |
| `#bugsGroup` | Bugs |
| `#cleanCodeGroup` | Clean Code |
| `#codingIdiomViolationGroup` | Coding Idiom Violation |
| `#designFlawGroup` | Design Flaws |
| `#instanceMethodProtocolGroup` | Instance Method Protocol |
| `#methodProtocolNameGroup` | Method Protocol Name |
| `#optimizationGroup` | Optimization |
| `#potentialBugsGroup` | Potential Bugs |
| `#rubricGroup` | Rubric |
| `#specGroup` | Spec |
| `#styleGroup` | Style |
| `#sunitGroup` | SUnit |
| `#unclassifiedMethodsGroup` | Unclassified rules (the default) |

The settings of the Quality Assistant use these groups to let the user enable or disable rules.

You can also specify the severity of your rule by returning one of the strings `'information'`, `'warning'` (the default) or `'error'` from the class-side `severity` method.

#### Unique Identifier
The class-side `uniqueIdentifierName` method answers the identifier used by the manifests to store the critiques banned by the user. It answers the name of the class by default. It should change only when the rule completely changes its semantics.

#### Documenting a Rule
The class comment of a rule is what developers read first. Besides the description of the rule, it should show a short code example of a violation and the corrected version, as well as how to execute the rule. A rule class does not need class-side `example…` (`<sampleInstance>`) or `script…` (`<script>`) methods: the code examples go in the class comment.

#### A Complete Example

```st
ReAbstractRule << #ReNoClassCommentRule
	package: 'General-Rules'

ReNoClassCommentRule class >> checksClass
	^ true

ReNoClassCommentRule class >> ruleName
	^ 'No class comment'

ReNoClassCommentRule class >> rationale
	^ 'Classes should have comments to explain their purpose, collaborations with other classes, and optionally provide examples of use.'

ReNoClassCommentRule class >> groupSelector
	^ #codingIdiomViolationGroup

ReNoClassCommentRule >> basicCheck: aClass
	(aClass isMeta or: [ aClass isTestCase ]) ifTrue: [ ^ false ].
	^ aClass hasComment not
```

### Testing

It is fairly easy to run your rule and obtain the results. Just create an instance of it and send it the `check:` message with the entity you want to check. The result is a collection of critiques. For example inspecting

```st
ReExcessiveMethodsRule new check: Object
```

should give you a collection with one critique (because the Object class always has many methods ;) ). Go on click on the critique item and inspect it. You will see that there is a special "description" tab. This is the power of critique objects, they can present themselves in a different way.

Pay attention that a rule checking AST nodes expects a node, not a method: `check:` on a method answers no critique. Use `ReSmalllintChecker` (see *Running Rules*) to run such a rule on methods, classes or packages.

#### Writing Tests
Rule tests subclass `ReAbstractRuleTestCase` and are named after the rule followed by `Test` (`ReAsClassRuleTest` tests `ReAsClassRule`): `subjectUnderTest` uses this convention to find the rule. Write in the test class a method that violates the rule and one that does not, and check them with `myCritiquesOnMethod:`, which also handles node rules. `myCritiquesOnClass:` does the same for class rules. Methods compiled by `generateMethod:` are removed after the test.

```st
ReAsClassRuleTest >> methodWithAsClassIfAbsent
	self class asClassIfAbsent: [  ]

ReAsClassRuleTest >> testRuleWithAsClassIfAbsent
	self assert: (self myCritiquesOnMethod: self class >> #methodWithAsClassIfAbsent) size equals: 1

ReAsClassRuleTest >> testRuleNotViolated
	self assertEmpty: (self myCritiquesOnMethod: self class >> #testRuleNotViolated)
```

#### Reset Cache
`ReRuleManager` caches the rules used by the Quality Assistant and the other tools. The cache is reset automatically when a rule class is added or removed. But a rule is usually created incomplete (for example, before `checksMethod` is defined it is not visible), so you have to reset the cache once your rule is ready by executing

```st
ReRuleManager reset
```

#### Special Rules
There are certain special rules with predefined functionality that allow you to easily perform complex checks.

##### ReNodeMatchRule
The base rule for code pattern matching (relies on rewrite expressions). The rule operates on AST nodes.

Use the following methods in the `initialize` method to setup your subclass:

- `matches:` add a string of rewrite expression to be matched by the rule
- `addMatchingExpression:` same as `matches:`
- `matchesAny:` same as previous but takes a collection of strings to match
- `addMatchingMethod:` add a string of rewrite expression which should be parsed as a method

You may override `afterCheck:mappings:` to do a post-matching validation of a matched node. The first argument is the matched node, the second one is a dictionary of the bindings of the wildcards. The dictionary can be accessed with the pattern nodes or with their pattern strings. For example:

```st
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

##### ReNodeRewriteRule
The base rule for code match and rewrite rules. The rule operates on AST nodes and its critiques propose a change that rewrites the matched code.

Use the following methods in the `initialize` method to setup your subclass:

- `replace:with:` (or `addMatchingExpression:rewriteTo:`) add a pair of rewrite expressions: the one to match and the one to replace the matched node with
- `replaceMethod:with:` (or `addMatchingMethod:rewriteTo:`) same as the previous one, but the rewrite expressions are parsed as methods
- `replace:byEvaluating:` (or `addMatchingExpression:rewriteWith:`) add a rewrite expression to match and a block that answers the node replacing the matched one. The block may accept 2 arguments: the matched node and the dictionary of wildcard mappings
- `addMatchingMethod:rewriteWith:` same as the previous one, but the rewrite expression is parsed as a method

As for `ReNodeMatchRule`, `afterCheck:mappings:` can be overridden to validate a match.


## Running Rules
Usually you don't have to run rules yourself, this is done by tools. But it is a good idea to be familiar with the API available in rules for developing your own tools and debugging unexpected behaviour.

First of all tools traverse the hierarchy of rule classes and select the required ones by testing them with `checksMethod`, `checksClass`, etc…

The main method that the rules implement to check entities is `check:forCritiquesDo:`. It accepts the entity to be checked as the first argument, and the block to be evaluated for each critique. The block may accept one parameter which is a critique object. For example:

```st
rule
	check: anEntity
	forCritiquesDo: [ :critique |
		"do something for each critique" ]
```

There are also 3 convenience methods:

- `check:` accepts an entity to check and returns the collection of critiques.
- `check:ifNoCritiques:` similarly to the previous one checks an entity and returns the collection of critiques. Additionally accepts a block that will be evaluated if no critiques are found.
- `check:forCritiquesDo:ifNone:` similarly to `check:forCritiquesDo:` checks an entity and evaluates the block for each detected critique. Additionally accepts a block that will be evaluated if no critiques are found.

### Running a Rule on a Class or a Package
`ReSmalllintChecker` runs rules on an environment and dispatches each entity to the rules interested in it: it checks the package, the classes, the methods and the AST nodes of the methods. It is the simplest way to run one rule, whatever the kind of entities it checks, on a class or a package.

```st
| rule checker |
rule := ReFloatEqualityComparisonRule new.
checker := ReSmalllintChecker new
	rule: { rule };
	environment: (PackageOrganizer default packageNamed: 'Kernel') asEnvironment;
	yourself.
checker run.
checker criticsOf: rule
```

Use `Point asEnvironment` instead of the package environment to check only the class `Point` and its methods. `runRules:onPackage:` runs a collection of rules on a package, for example the default rules; `criticsOf:` then answers the critiques of one of them:

```st
| checker |
checker := ReSmalllintChecker new.
checker
	runRules: ReRuleManager defaultRules
	onPackage: (PackageOrganizer default packageNamed: 'Kernel').
checker criticsOf: ReFloatEqualityComparisonRule new
```

Finally, `critiques` runs all the enabled rules on a class, a method or a package, as the Quality Assistant does, and takes into account the critiques banned in the manifests:

```st
Point critiques.
(Point >> #degrees) critiques
```

`critiques` relies on `ReCriticEngine`: `ReCriticEngine critiquesOf: anEntity` runs the method, class and package rules, and `ReCriticEngine nodeCritiquesOf: aMethod` runs the node rules on the AST of a method. The critiques of a method are the union of both. Keep the critiques of one rule with `select: [ :critique | critique rule class == ReFloatEqualityComparisonRule ]`.

## Critiques

A critique is an object that binds an entity with a rule that is violated by that entity. The critique describes a specific violation, and may provide a solution to fix it.

### Critique Design
This section contains explanations of the design behind critiques.

#### Class Hierarchy
`ReAbstractCritique` is the root of the critiques hierarchy.

`ReAbstractCritique` inherits from `ReProperty` which represents an external property of some entity related to a piece of code. It defines a basic interface of a title and an icon that can be used to display it in a user interface. It also has a source anchor pointing to the piece of code.

#### Rule Reference
A critique has a reference to the rule that reported the violation.
The rule's `ruleName` is used as the critique's `title`, its `severity` defines the critique's `icon`, and the rule's `rationale` is used as the `description` of the critique.

#### Entity Reference
A critique has a reference to the criticized entity.

This link is established through `ReSourceAnchor`. A source anchor has a reference to the actual class, method, or other entity that is criticized. An anchor also has a `providesInterval` method that returns a boolean indicating if the anchor provides a selection interval to the actual source of the critique. The interval can be accessed through the `interval` method.

There are two subclasses of `ReSourceAnchor`.

- `ReIntervalSourceAnchor` stores the actual interval object which is set during initialization.
- `ReSearchStringSourceAnchor` stores a searchString which will be searched for in the entity's source code on demand to find an interval of substring.

#### Automated Fixes
A critique has the `providesChange` method which returns a boolean value specifying whether the critique can provide a change which will resolve the issue.

The `change` method can be used to obtain an object of `RBRefactoryChange` kind.

#### Custom Actions
One can override the `actions` method to return a list of `RePropertyAction` objects. Tools can use these objects to provide a user with custom actions defined by critiques themselves.

#### RePropertyAction
I am an action that appears in the browser QA plugin next to the item's title.

- icon - a Form that will appear on the button (green square by default)
- description - the description that will be present on popup on hover
- action - a two (optional) parameter block that is evaluated with the critique and the current code entity (class, method…) when the button is pressed.


## Migrating Rules To Renraku
Pharo tools moved to the Renraku framework which requires a slightly different implementation from rules. All the rules of Pharo are now Renraku rules: `RBParseTreeLintRule` and `RBTransformationRule` (package `Refactoring-Critics`) have no subclasses anymore. This section is kept to help you convert rules written for older versions of Pharo.

### Generic Rules
To convert a generic rule (one that simply checks a method, a class or a package) to work with Renraku you have to follow 3 simple steps:

#### Inheritance:
Change the superclass to `ReAbstractRule`.

#### Interest:
Specify which entities your rule is interested in by overriding a method on the class side to return true. If the rule was checking methods override `checksMethod`, for classes override `checksClass`, and for packages `checksPackage`.

#### Checking:
Implement `basicCheck:` in a way that it will return true if the argument violated the rule and false otherwise. The quality tools will pass you only the arguments of the type you've expressed interest in.

Also move the name of the rule to the class-side `ruleName` method and its group to the class-side `groupSelector` method.

### Parse Tree Rules

To convert parse tree rules (subclasses of `RBParseTreeLintRule`) please change their superclass to `ReNodeMatchRule`.

Then change the initialization method. Instead of sending match-specifying methods to `matcher`, send them to self. The rest of the API is similar:

- `matches:do:` -> `matches:`
- `matchesMethod:do:` -> `addMatchingMethod:`
- `matchesAnyOf:do:` -> `matchesAny:`

So the old initialization:

```st
	self matcher
		matches: '`var := `var'
		do: [ :node :answer | node ]
```

will become:

```st
	self matches: '`var := `var'
```

You might have noticed that the new API is missing the `do:` part. First of all almost no rules use this functionality and you can check a node in the matching expression with the `{:node | "check node" }` syntax.

But the new rules also give you a more powerful way of post-checking matched nodes. You can override the `afterCheck:mappings:` method and return true if the node really violates the rule or false otherwise (see `ReNodeMatchRule` above).

### Rewrite Rules
To convert rewrite rules (subclasses of `RBTransformationRule`) please change their superclass to `ReNodeRewriteRule`.

Then change the initialization method. Instead of sending transformation-specifying methods to `rewriteRule`, send them to self. The rest of the API is similar:

- `replace:with:` -> `replace:with:` (no change)
- `replaceMethod:with:` -> `replaceMethod:with:` (no change)
- `replace:withValueFrom:` -> `replace:byEvaluating:` (the second argument is a block which accepts the matched node and returns a node that should be used for replacement)

So the old initialization:

```st
	self rewriteRule
		replace: '`var := `var' with: ''
```

becomes:

```st
	self replace: '`var := `var' with: ''
```

The new rules also give you a more powerful way of post-checking matched nodes with `afterCheck:mappings:`, as for `ReNodeMatchRule`.
