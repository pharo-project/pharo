matching
copyList: matchNodes inContext: aDictionary 
	| newNodes |
	newNodes := OrderedCollection new.
	matchNodes do: 
			[:each | 
			| object |
			object := each copyInContext: aDictionary.
			newNodes addAll: object].
	^newNodes