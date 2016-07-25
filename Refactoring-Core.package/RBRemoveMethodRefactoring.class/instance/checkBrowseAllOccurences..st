preconditions
checkBrowseAllOccurences: anCollectionOfOccurrences
	| methods callers |
	methods := anCollectionOfOccurrences collect: [ :c | c key ] as: Set.
	callers := anCollectionOfOccurrences collect: [ :v | v value ].
	methods size = 1
		ifTrue: [ 
			self
				refactoringError:
					('Possible call to <2s> in <1p> methods.<n>Browse references?'
						expandMacrosWith: anCollectionOfOccurrences size
						with: methods anyOne)
				with: [ self openBrowserOn: (RBBrowserEnvironment new referencesTo: methods anyOne) ] ]
		ifFalse: [ 
			self
				refactoringError:
					('Possible call to the <2p> selectors in <1p> methods.<n>Browse references?'
						expandMacrosWith: callers size
						with: methods size)
				with: [ 
					| env |
					env := RBSelectorEnvironment new.
					callers do: [ :d | env addMethod: d method ].
					self openBrowserOn: env ] ]