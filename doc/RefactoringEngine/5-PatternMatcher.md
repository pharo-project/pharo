## RBPatternParser and metavariables

Generating an AST of Smalltalk source code and implementing a program node visitor gives already great and powerful capabilities. The refactoring framework extends this expressiveness by including so called "metavariables".

As these expressions are using an extended syntax - metavariables aren't known to the `RBParser` - a special parser is needed to parse this expression, the `RBPatternParser`. The following pages describe the added syntax elements. 

Metavariables are a part of a parser expression, just like any other Smalltalk code, but instead of representing an expression with the exact name, they form a variable that can be unify with any real code expression with the same *structure*.

### An example:

Parsing an expression like:

```st
a := a + 1 
```

creates a parse tree with an assignment node assigning to 'a', the value of sending the message '+' with argument 1 to the object 'a'.

We could implement a refactoring operation (or directly use the RBParseTreeSearcher/Rewriter) to create a refactoring  for this kind of code. But of course, it would only work for code using this variable name.

We can define the expression with the meaning of 'increment a variable by one' by using a metavariable. All metavariables start with a ´ (backquote).

```
`a := `a + 1
```
This is the simplest metavariable, a name with a backquote. It will match a single variable. And for matching the whole expression, all variables with the same name must match the same variables. 

The above expression only matches 

```
'x:=x+1'
```

but not 

```
'x:=y+1'.
```

If we want to match more than a single variable, we can prefix the name with a '@':

```
`a
``` 
matches a single variable

```
`@a 
``` 
matches multiple items in this position

For example,

```
`@a add: `@b
```

will match any expression with the message send #add: regardless whether the receiver or arguments are single variables

```
'coll add: item'
```

or the return of another expression

```
'self data add: self'
```

Furthermore, we can restrict the expression to be matched to be a literal instead of variable by using the prefix '#':

```
`@exp add: `#item
```

This will match any code calling #add: on an object or expression with a literal as argument:

```
'coll add: 3'
'self foo add: 'text' '
'coll add: #symbol'
```

But again, #lit is a named variable and matches only the same literal in every part of the expression:

```
`self add: `#lit; add: `#lit
```
will match

```
'self add: #a; add: #a'
```
but not 

```
'self add: #a; add: #b'
```

Similar to a statement ending with a dot, the metavariable prefix `'.'` defines a variable matching a statement, resp. `'.@'` a (possible empty) list of statements.

Example, match `ifTrue:ifFalse:` with first statement in true and false block being the same

```
`@exp ifTrue:[`.stm. 
				  `.@trueCase]
      ifFalse:[`.stm. 
				  `.@falseCase]
```

This will match

```st
someValue ifTrue:[ self doA.
	                self doFoo]
          ifFalse:[ self doA.
	                self doBaz]

```

Important especially for the rewriter, we may not only want to know the first node matching an expression but every other and for example any possible subexpression matching the metavariable. For this, we can
use a double backquote to indicate that the search should recurse into the found node expression to search for more matches.

This expression will find all senders of add:

```
`@exp add:`@value
```

but if we would use this expression to rewrite add: by addItem:
an expression like

```st
var add: (self add: aValue).
```

would be replaced by

```st
var addItem: (self add: aValue).
```

If we want to find the same call in the argument, we need to recurse into it by using a double backquote

```
`@exp add:``@value
```

## Examples and usage of RBPatternParser expressions

The chapter "RBPatternParser and metavariables" describes the added syntax elements for the RBPatternParser used in the refactoring engine (RBParseTreeSearcher/RBParseTreeRewriter).

In this chapter we show some example expressions and how to test and use them.

Calypso has a search function that is the simplest way to use and see the result of searching expressions with pattern syntax. Open the the class menu / Refactoring / Code Rewrite / Search code or Rewrite code entry.

Search code
The search code menu will put a search pattern template in the code pane:

```st
RBParseTreeSearcher new
	matches: '`@object' do: [ :node :answer | node ];
	matchesMethod: '`@method: `@args | `@temps | `@.statements' do: [ :node :answer | node ];
	yourself
```	

This template defines two match rules, one for the code search 'matches:' and one for the named method search 'matchesMethod', the former looks for expression in any method while the latter one matches whole methods.

You can replace the example pattern

```
`@object`
```

or 

```
`@method: ``@args | `@temps | `@.statements
```

by the search pattern you want to use.

And most of the time you only want to use one, the code expression search or the method search.

A first example, replace the code pane content by:

```st
RBParseTreeSearcher new
	matchesMethod: 'drawOn: `@args | `@temps | `@.statements' do: [ :node :answer | node ];
	yourself
```

You can now accept this code, instead of saving this method it will just spawn a code searcher trying all defined methods to match against this pattern and opens a MessageBrowser for all found results.
The result is actually the same as if we had searched for all implementors of #drawOn:

Next example, replace the code pane content by:

```st
RBParseTreeSearcher new
	matches: '`@object drawOn: `@args' do: [ :node :answer | node ];
	yourself
```

The result is similar to looking for senders of `drawOn:` (not the same actually, as sendersOf also looks for methods containing the symbol `#drawOn:`).

The `do:` block can be used to further test or filter the found matches. The node is the current matched node and the answer is not needed here. It is important that for every entry you want to include in the result to return "the node" and for everything else return "nil"

Example, search for all methods with at least one argument where the method name starts with `'perform'`:

```st
RBParseTreeSearcher new
		matchesMethod: '`@method: `@args | `@temps | `@.statements'
			do: [ :node :answer | 
			((node selector beginsWith: 'perform') and: [ node arguments isEmpty not ])
				ifTrue: [ node ]
				ifFalse: [ nil ] ];
		yourself
```

Another way to use extended pattern syntax is to directly instantiate a `RBParseTreeSearcher` and execute it on a parse tree.
First we define the pattern, instantiate a tree searcher and tell him what to do when matching this pattern (just return the matched node) and execute it on the AST of Numbers method #asPoint.

```st
| searcher pattern parseTree |
pattern := '^ self'.
searcher := RBParseTreeSearcher new.
searcher matches: pattern do:[:node :answer |node].
searcher executeTree: (Number>>#asPoint) ast initialAnswer: nil.
```

it will return nil, since no node in that method returns 'self'. If we execute the searcher instead on the method for class `Point`, it will return the found node, a `RBReturnNode`

```st
searcher executeTree: (Point>>#asPoint) ast initialAnswer: nil.
```

If we don't just want to match an expression but collecting all matching nodes, we can collect all nodes within the #do: block:

```st
| searcher pattern parseTree  selfMessages |
selfMessages := Set new.
pattern := 'self `@message: ``@args'.
searcher := RBParseTreeSearcher new.
searcher matches: pattern do:[:node :answer |  selfMessages add: node selector].
searcher executeTree: (Morph>>#fullDrawOn:) ast initialAnswer: nil.
selfMessages inspect.
```

This will collect all messages send to self in method Morph>>#fullDrawOn:

### RBBrowserEnvironment

The first and main use for browser environments are to restrict the namespace in which a refactoring operation is applied. For example, if you want to rename a method and and update all senders of this method, but only in a certain package, you can create a RBNamespace from a scoped 'view' of the classes from the whole system. Only the classes in this restricted environment are affected by the transformation.

In the meantime other tools are using this environment classes as well. Finder, MessageBrowser or the SystemBrowser can work with a scoped environment to show and operate only on classes and methods in this environment.

There are different subclasses of RBBrowserEnvironment for the different kind of 'scopes'. 

- RBClassEnvironment - only show classes/methods from a set of classes.
- RBPackageEnvironment - only show classes / packages / methods from a set of packages.
- RBSelectorEnvironment - only show classes / methods from a set of selector names.

Instead of directly using the different subclasses for a scoped view, the base class `RBBrowserEnvironment` can act as a factory for creating restricted environments. See the methods in its 'environments'-protocol, on how to create the different environments.

You start with a default environment containing all classes from the system and create a new scoped environment by calling the appropriate method.

For example, creating an environment for all classes in package 'Kernel':

```st
RBBrowserEnvironment new forPackageNames:{'Kernel'}.
```

You can query the environment.

```st
| env |
env := RBBrowserEnvironment new forPackageNames:{'Kernel'}.
env allClasses 
-> a list of all classes in package Kernel
```

or open a browser
env browse "-> starts Calypso showing only this package"

and you can further restrict this package environment by calling one of the other factory methods:

```st
env class 
-> a RBPackageEnvironment
```

```st
(env implementorsOf:#collect:) class
->  RBSelectorEnvironment
```

Another way to combine or further restrict environments is to use boolean operations and, not or or.

```st
| implDrawOn callsDrawOn implAndCalls |
callsDrawOn := RBBrowserEnvironment new referencesTo: #drawOn:.
implDrawOn :=  RBBrowserEnvironment new implementorsOf: #drawOn:.
"create an 'anded'-environment"
implAndCalls := callsDrawOn & implDrawOn.
"collect all message and open a MessageBrowser"
MessageBrowser browse: implAndCalls methods.
```

This opens a MessageBrowser on all methods in the system that implement `#drawOn:` and calls `drawOn:`.

```st
| implPrintOn notImplPrintOn |
implPrintOn := RBBrowserEnvironment new implementorsOf: #printOn:.
"create a 'not'-environment"
notImplPrintOn := implPrintOn not.
implPrintOn includesClass: Object. 
-> true
notImplPrintOn includesClass: Object. 
-> false
```

Classes implementing `#printOn:` are not in the 'not'-environment.

A more generic way to create an environment by giving an explicit 'test'-block to select methods for this environment:

```st
|implementedByMe|
implementedByMe := RBBrowserEnvironment new selectMethods:[:m | m selector size > 10 ].
implementedByMe browse.
```

This opens (may be slow) a browser with all classes with methods having a selector larger than 10 characters.
