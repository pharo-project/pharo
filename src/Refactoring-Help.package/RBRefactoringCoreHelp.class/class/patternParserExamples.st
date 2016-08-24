pages
patternParserExamples
	^HelpTopic
		title: 'Examples and usage of RBPatternParser expressions'
		contents: 
'
The chapter "RBPatternParser and metavariables" describes the added syntax elements for the RBPatternParser used in the refactoring engine (RBParseTreeSearcher/RBParseTreeRewriter).

In this chapter we show some example expressions and how to test and use them.

Nautilus has a search function that is the simples way to use and see the result of searching expressions with pattern syntax. Open the the class menu / Refactoring / Code Rewrite / Search code or Rewrite code entry.

Search code
The search code menu will put a search pattern template in the code pane:

RBParseTreeSearcher new
	matches: ''`@object'' do: [ :node :answer | node ];
	matchesMethod: ''`@method: `@args | `@temps | `@.statements'' do: [ :node :answer | node ];
	yourself
	

This template defines two match rules, one for the code search ''matches:'' and one for the named method search ''matchesMethod'', the former looks for expression in any method while the latter one matches whole methods.

You can replace the example pattern ''`@object'' or ''`@method: `@args | `@temps | `@.statements'' by
the search pattern you want to use. And most of the time you only want to use one, the code expression search or the method search.

A first example, replace the code pane content by:
RBParseTreeSearcher new
	matchesMethod: ''drawOn: `@args | `@temps | `@.statements'' do: [ :node :answer | node ];
	yourself

You can now accept this code, instead of saving this method it will just spawn a code searcher trying all defined methods to match against this pattern and opens a MessageBrowser for all found results.
The result is actually the same as if we had searched for all implementors of #drawOn:

Next example, replace the code pane content by:
RBParseTreeSearcher new
	matches: ''`@object drawOn: `@args'' do: [ :node :answer | node ];
	yourself

The result is similar to looking for senders of #drawOn: (not the same actually, as sendersOf also looks for methods containing the symbol #drawOn: )	
	
The #do: block can be used to further test or filter the found matches. The node is the current matched node and the answer is not needed here. It is important that for every entry you want to include in the result to return "the node" and for everything else return "nil"

Example, search for all methods with at least one argument where the method name starts with ''permform'':

RBParseTreeSearcher new
		matchesMethod: ''`@method: `@args | `@temps | `@.statements''
			do: [ :node :answer | 
			((node selector beginsWith: ''perform'') and: [ node arguments isEmpty not ])
				ifTrue: [ node ]
				ifFalse: [ nil ] ];
		yourself

Another way to use extended pattern syntax is to directly instantiate a RBParseTreeSearcher and execute it on a parse tree.
First we define the pattern, instantiate a tree searcher and tell him what to do when matching this pattern (just return the matched node) and execute it on the AST of Numbers method #asPoint.

| searcher pattern parseTree |
pattern := ''^ self''.
searcher := RBParseTreeSearcher new.
searcher matches: pattern do:[:node :answer |node].
searcher executeTree: (Number>>#asPoint) ast initialAnswer: nil.

it will return nil, since no node in that method returns ''self''. If we execute the searcher instead on the method
for class Point, it will return the found node, a RBReturnNode

searcher executeTree: (Point>>#asPoint) ast initialAnswer: nil.

If we don''t just want to match an expression but collecting all matching nodes, we can collect all nodes within the #do: block:

| searcher pattern parseTree  selfMessages |
selfMessages := Set new.
pattern := ''self `@message: ``@args''.
searcher := RBParseTreeSearcher new.
searcher matches: pattern do:[:node :answer |  selfMessages add: node selector].
searcher executeTree: (Morph>>#fullDrawOn:) ast initialAnswer: nil.
selfMessages inspect.

This will collect all messages send to self in method Morph>>#fullDrawOn:


'