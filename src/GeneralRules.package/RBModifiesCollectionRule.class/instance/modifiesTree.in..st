private
modifiesTree: aCollectionTree in: aParseTree 
	| notifier args |
	notifier := RBParseTreeSearcher new.
	args := Array with: (RBPatternVariableNode named: '`@object').
	notifier 
		matchesAnyTreeOf: (#(add: addAll: remove: removeAll:) collect: 
					[:each | 
					RBMessageNode 
						receiver: aCollectionTree copy
						selector: each
						arguments: args])
		do: [:aNode :answer | true].
	^notifier executeTree: aParseTree initialAnswer: false