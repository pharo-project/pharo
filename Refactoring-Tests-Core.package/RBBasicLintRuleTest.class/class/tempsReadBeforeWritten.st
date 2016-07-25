possible bugs
tempsReadBeforeWritten
	| detector |
	detector := self new.
	detector name: 'Temporaries read before written'.
	detector methodBlock: 
			[:context :result | 
			| variables |
			variables := RBParseTreeSearcher nonBlockTempsIn: context parseTree.
			variables isEmpty 
				ifFalse: 
					[(RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: context parseTree) 
						do: 
							[:each | 
							result addClass: context selectedClass selector: context selector.
							result addSearchString: each]]].
	^detector