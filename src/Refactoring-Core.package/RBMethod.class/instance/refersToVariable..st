testing
refersToVariable: aString 
	| searcher tree |
	tree := self parseTree.
	((tree defines: aString) or: [tree body defines: aString]) 
		ifTrue: [^false].
	searcher := RBParseTreeSearcher new.
	searcher
		matches: aString do: [:node :answer | true];
		matches: '[:`@vars | | `@temps | `@.Stmts]'
			do: 
				[:node :answer | 
				answer or: 
						[((node defines: aString) or: [node body defines: aString]) not 
							and: [searcher executeTree: node body initialAnswer: false]]].
	^searcher executeTree: self parseTree initialAnswer: false