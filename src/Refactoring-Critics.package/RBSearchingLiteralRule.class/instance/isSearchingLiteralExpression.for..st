private
isSearchingLiteralExpression: aSearchingNode for: anObjectNode 
	| argument arguments |
	aSearchingNode isMessage ifFalse: [^false].
	arguments := aSearchingNode arguments.
	arguments size = 1 ifFalse: [^false].
	argument := arguments first.
	(#(#= #==) includes: aSearchingNode selector) 
		ifTrue: 
			[^(aSearchingNode receiver = anObjectNode 
				and: [aSearchingNode arguments first isLiteralNode]) or: 
						[aSearchingNode arguments first = anObjectNode 
							and: [aSearchingNode receiver isLiteralNode]]].
	aSearchingNode selector = #| 
		ifTrue: 
			[^(self isSearchingLiteralExpression: aSearchingNode receiver
				for: anObjectNode) 
					and: [self isSearchingLiteralExpression: argument for: anObjectNode]].
	aSearchingNode selector = #or: ifFalse: [^false].
	argument isBlock ifFalse: [^false].
	argument body statements size = 1 ifFalse: [^false].
	^(self isSearchingLiteralExpression: aSearchingNode receiver
		for: anObjectNode) and: 
				[self isSearchingLiteralExpression: argument body statements first
					for: anObjectNode]