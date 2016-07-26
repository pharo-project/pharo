I am an abstract superclass for ParseNode visitors that functions as a null visitor.  Here's the code that defines my interface:

(SystemNavigation new allImplementorsOf: #accept: localTo: ParseNode) do:
	[:methodReference|
	methodReference compiledMethod messages do:
		[:sel|
		((sel beginsWith: 'visit')
		and: [sel numArgs = 1]) ifTrue:
			[ParseNodeVisitor
				compile: (String streamContents:
							[:str|
							str nextPutAll: sel;
								space;
								nextPut: $a.
							methodReference classSymbol first isVowel ifTrue:
								[str nextPut: $n].
							str nextPutAll: methodReference classSymbol])
				classified: 'visiting']]]