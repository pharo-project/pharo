pages
patternParser
	^HelpTopic
		title: 'RBPatternParser and metavariables'
		contents: 
'
Generating an AST of Smalltalk source code and implementing a program node visitor gives already great and powerful capabilities. The refactoring framework extends this expressiveness by including so called "metavariables".

As this expressions are using an extended syntax - metavariables aren''t known to the RBParser - a special parser is needed to parse this expression, the RBPatternParser.
The following pages describe the added syntax elements. Examples on how to use or tests these expressions
can be found in the chapter "RBPatternParser examples".

metavariables are a part of a parser expression, just like any other Smalltalk code, but instead of representing an expression with the exact name, they form a variable that can be unify with any real code expression with the same *structure*.

An example:
Parsing an expression like:
a := a + 1 
creates a parse tree with an assignment node assigning to ''a'', the value of sending the message ''+'' with argument 1 to the object ''a''.

We could implement a refactoring operation (or directly use the RBParseTreeSearcher/Rewriter) to create a refactoring  for this kind of code. But of course, it would only work for code using this variable name.

We can define the expression with the meaning of ''increment a variable by one'' by using a metavariable. All metavariables start with a ´ (backquote).
`a := `a + 1

This is the simplest metavariable, a name with a backquote. It will match a single variable. And for matching the whole expression, all variables with the same name must match the same variables. 
The above expression only matches 
''x:=x+1'' 
but not 
''x:=y+1''.

If we want to match more than a single variable, we can prefix the name with a ''@'':

`a matches a single variable
`@a matches multiple items in this position

For example, 
`@a add: `@b
will match any expression with the message send #add: regardless whether the receiver or arguments are single variables
''coll add: item''
or the return of another expression
''self data add: self''

Furthermore we can restrict the expression to be matched to be a literal instead of variable by using the prefix ''#'':

`@exp add: `#item

This will match any code calling #add: on an object or expression with a literal as argument:
''coll add: 3''
''self foo add: ''text'' ''
''coll add: #symbol''

But again, #lit is a named variable and matches only the same literal in every part of the expression:

`self add: `#lit; add: `#lit

will match
''self add: #a; add: #a''
but not 
''self add: #a; add: #b''

Similar to a statement ending with a dot, the metavariable prefix ''.'' defines a variable matching a statement, resp. ''.@'' a (possible empty) list of statements.

Example, match ifTrue:ifFalse: with first statement in true and false block being the same

`@exp ifTrue:[`.stm. 
				  `.@trueCase]
      ifFalse:[`.stm. 
				  `.@falseCase]

This will match

someValue ifTrue:[ self doA.
	                self doFoo]
          ifFalse:[ self doA.
	                self doBaz]


Important especially for the rewriter, we may not only want to know the first node matching an expression but every other and for example any possible subexpression matching the metavariable. For this, we can
use a double backquote to indicate that the search should recurse into the found node expression to search for more matches.

This expression will find all senders of add:
`@exp add:`@value
but if we would use this expression to rewrite add: by addItem:
an expression like

var add: (self add: aValue).

would be replaced by

var addItem: (self add: aValue).

If we want to find the same call in the argument, we need to recurse into it by using a double backquote

`@exp add:``@value
'