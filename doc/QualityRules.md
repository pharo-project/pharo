# Renraku Quality Rules

Renraku is a framework for defining and processing quality rules. The framework operates with three main concepts: entities, rules and critiques.

### Entities
Entities are not a part of Renraku, but Renraku is validating entities. Theoretically any object can be an entity, but in practice we mostly focus on code entities such as methods, classes, packages, AST nodes.

### Rules
Rules are the objects that describe constraints about entities. A rule can check an entity and produce critiques that describe the violations of the entity according to the rule.

###  Critiques
Critique is an object that binds an entity with a rule that is violated by that entity. The critique describes a specific violation, and may provide a solutions to fix it.


## Rules
Rules are constraint descriptions about entities. Think of a rule as a function that consumes an entity and produces a collection of critiques about it (which can be empty if there are no violations).

### Creating Rules
Each rule has a few fundamental features to implement in this book with will walk through all the steps of rule creation.
	
Make sure to RESET the CACHE after you add a rule. (last chapter)

#### Subclass
Subclass `ReAbstractRule` or one of the special rule classes that will be discussed. Most of tools rely on the classes hierarchy to select the rules for checking.

#### Name and Rationale
Each rule should provide a short name string returned from the `name` method. You also have to override the `rationale` method to return a detailed description about the rule. You may also put the rationale in the class comment, as by default `rationale` method returns the comment of the rule's class.

#### Specifying an Interest in Entities
The class-side methods `checksMethod`, `checksClass`, `checksPackage` and `checksNode` return true is the rule checks methods, classes or traits, packages and AST nodes respectively. Tools will pass entities of the specified type to the rule for checking. A rule may check multiple types of entities but please avoid checks for types inside of rules. E.g. if a rule checks whether an entity is named with a swearing word and does this by obtaining the name of the entity and matching substrings. It is completely fine to specify that the rule checks classes and packages as you don't have to distinguish which entity was passed to you.

#### Checking
The easiest way to check an entity is to override the `basicCheck:` method. The method accepts an entity as a parameter and returns true if there is a violation and false otherwise.

#### Advanced Checking
While there is a default implementation which relies on `basicCheck:` and creates an instance of `ReTrivialCritique`, it is advised to override the `check:forCritiquesDo:` method. This method accepts an entity and a block which could be evaluated for each detected critique. This means that one rule can detect multiple critiques about one entity. For example if a rule checks for unused variables it can report all of them with a dedicated critique for each.

The block which should be evaluated for each critique may accept one argument: the critique object, this is why you have to evaluate it with `cull:`. You may use the `critiqueFor:` method if you don't feel comfortable with critiques yes. For example:

```st
self critiqueFor: anEntity
```

will return `ReTrivialCritique` about the entity. Later you can update your code to create other kinds of critiques more suitable for your case.

### Testing

It is fairly easy to run your rule and obtain the results. Just create an instance of it an send it the `check:` message with the entity you want to check. The result is a collection of critiques. For example inspecting

```st
RBExcessiveMethodsRule new check: Object
```

should give you a collection with one critique (because the Object class always has many methods ;) ). Go on click on the critique item and inspect it. You will see that there is a special "description" tab. This is the power of critique objects, they can present themselves in a different way. Guess what: you can even visualize the critique if needed.

#### Group and Severity
It's a good idea to assign your rule to a specific group. For this override the #group method and return string with the name of the group. While you can use any name you want, maybe you would like to put your rule into one of the existing groups: 

- API Change, 
- API Hints,
- Architectural, 
- Bugs, 
- Coding Idiom Violation, 
- Design Flaws, 
- Optimization, 
- Potential Bugs, 
- Rubric, 
- SUnit, 
- Style, and 
- Unclassified rules.

You can also specify the severity of your rue by returning one of: #information, #warning, or #error symbols from the #severity method.

#### Reset Cache
To have quality assistant (and maybe other tools) pick up your changes you have to reset the cache. Do this by going to

System > Settings > Code Browsing > QualityAssistant > Renraku > Rule Cache

and pressing the reset button. Or simply executing

```st
ReRuleManager reset
```

When you load complete rules into the system, the cache will be reset automatically. But as you are creating a new rule and it is in the incomplete state you have to reset the cache once you are ready.

#### Special Rules
There are certain special rules with predefined functionality that allows to easily perform complex checks. The subsections of this Book contain references to the rules

##### ReNodeMatchRule
The base rule for code pattern matching (relies on rewrite expressions). The rule operates on AST nodes.

Use the following methods in the initialization to setup your subclass:

- `matches:`
- `addMatchingExpression:` add a string of rewrite expression to be matched by rule
- `matchesAny:` same as previous but takes a collection of strings to match
- `addMatchingMethod:` add a string of rewrite expression which should be parsed as a method
	
you may use `afterCheck:mappings:` to do a post-matching validation of a matched node and mapping of wildcards.


## Running Rules
Usually you don't have to run rules yourself, this is done by tools. But it is a good idea to be familiar with the API available in rules for developing your own tools and dubugging unexpected behaviour.
		
First of all tools traverse the hierarchy of rule classes and selects the required ones buy testing them with `checksMethod`, `checksClass`, etc…

The main method that the rules implement to check entities is `check:forCritiquesDo:`. It accepts the entity to be checked as the first argument, and the block to be evaluated for each critique. The block may accept one parameter which is a critique object. For example:

```st
rule
	check: anEntity
	forCritiquesDo: [ :critique |
		"do something for each critique" ]
```		
		
There are also 3 convenience methods:

- `check:` Accepts an entity to check and returns the collection of critiques.

- `check:ifNoCritiques:`
Similarly to previous one checks an entity and returns the collection of critiques. Additionally accepts a block that will be evaluated if no critiques are found.

- `check:forCritiquesDo:ifNone:`
Similarly to `check:forCritiquesDo:` checks an entity and evaluates the block for each detected critique. Additionally accepts a block that will be evaluated if no critiques are found.

## Critiques

Critique is an object that binds an entity with a rule that is violated by that entity. The critique describes a specific violation, and may provide a solutions to fix it.

### Critique Design
This book contains explanations of the design behind critiques

#### Class Hierarchy
`ReAbstractCritique` is the root of the critiques hierarchy.

`ReAbstractCritique` inherits from `ReProperty` which represents an external property of some entity related to a piece of code. It defines a basic interface of a title an and an icon that can be used to display it in a user interface. It also has a source anchor pointing the piece of code.

#### Rule Reference
A critique has a reference to the rule that reported the violation.
The rule's `name` is used as the critique’s `title` and the rule's `rationale` is used as the `description` of the critique.

#### Entity Reference
A critique has a reference to the criticized entity.

This link is established through `ReSourceAnchor`. A source anchor has a reference to the actual class, method, or other entity that is criticized. An anchor also has a `providesInterval` method that returns a boolean indicating if the anchor provides a selection interval to the actual source of the critique. The interval can be accessed through the `interval` method.

There are two subclasses of `ReSourceAnchor`.
ReIntervalSourceAnchor stores the actual interval object which is set during initialization.

`ReSearchStringSourceAnchor` stores a searchString which will be searched for in the entities source code on demand to find an interval of substring.

#### Automated Fixes
A critique has the `providesChange` method which returns a boolean value specifying whether the critique can provide a change which will resolve the issue.

The `change` method can be used to obtain an object of `RBRefactoryChange` kind.

#### Custom Actions
One can override #actions method to return a list of `RePropertyAction` objects. Tools can use these objects to provide a user with custom actions defined by critiques themselves.

#### RePropertyAction
I am an action that appears in the Nautiluas qa plugin next the the item's title.

- icon - a Form that will appear on the button (green square by default)
- description - the description that will be present on popup on hower
- action - a two (ortional) parameter block that is evaluated with the critic and the current code entity (class, method…) when the button is pressed.


## Migrating Rules To Renraku
Pharo tools moved to Renraku framework which requires a slightly different implementation from rules.
	
While you can achieve much more features by reading the whole documentation and using the complete set of Renraku possibilities, this book contains a few simple steps to help you converting existing rules to work with the Renraku model.

### Generic Rules
To convert a generic rule (one that simply checks method, class or package) to work with Renraku you have to follow 3 simple steps:

Inheritance:
change superclass to `ReAbstractRule`.

#### Interest:
Specify which entities your rules is interested in by overriding a method on the class side to return true. If the rule was checking methods override `checksMethod`, for classes override `checksClass`, and for packages `checksPackage`.

#### Checking:
Implement `basicCheck:` in a way that it will return true if the argument violated the rule and false otherwise. The quality tools will pass you only the arguments of the type you've expressed interest in.


### Parse Tree Rules

To convert parse tree rules (subclasses of `RBParseTreeLintRule`) please change their superclass to `ReNodeMatchRule`.

Then change the initialization method. Instead of sending match-specifying methods to `matcher`, send them to self. The rest of API is similar:
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

You might have noticed that new API is missing the `do:` part. First of all almost no rules use this functionality and you can check node in the matching expression with `{:node | "check node" } syntax.

But the new rules also give you a move powerful way of post-checking matched nodes. You can override `afterCheck:mappings:` method and return true if node really violates the rule or false otherwise. The first argument passed to the method is the matched node object, while the second argument is a dictionary of bindings for the wildcards in the rule. For example if the pattern '`var := `var' will match expression 'a := a' the matches dictionary will contain one entry where key is ASTPatternVariableNode(`var) and value is RBVariableNode(a).

P.S. at the moment of writing the matches dictionary was not used in a real setting, so don't hesitate to provide a feedback. Maybe instead of node objects the dictionary should contain strings.

### Rewrite Rules
To convert parse tree rules (subclasses of `RBTransformationRule`) please change their superclass to `ReNodeRewriteRule`.

Then change the initialization method. Instead of sending transformation-specifying methods to #rewriteRule, send them to self. The rest of API is similar:
- `replace:with:` -> `replace:with:` (noChange)
- `replaceMethod:with:` -> `addMatchingMethod:rewriteTo:` -> `replace:by:` (second argument is a block which accepts matched node and returns a node that should be used for replacement).

So the old initialization:

```st
	self rewriteRule
		replace: 	'`var := `var' with: ''
```		

becomes:

```st
	self replace: 	'`var := `var' with: ''
```

The new rules also give you a move powerful way of post-checking matched nodes. You can override `afterCheck:mappings:` method and return true if node really violates the rule or false otherwise. The first argument passed to the method is the matched node object, while the second argument is a dictionary of bindings for the wildcards in the rule. For example if the pattern '`var := `var' will match expression 'a := a' the matches dictionary will contain one entry where key is ASTPatternVariableNode(`var) and value is `RBVariableNode(a)`.

P.S. at the moment of writing the matches dictionary was not used in a real setting, so don't hesitate to provide a feedback. Maybe instead of node objects the dictionary should contain strings.
