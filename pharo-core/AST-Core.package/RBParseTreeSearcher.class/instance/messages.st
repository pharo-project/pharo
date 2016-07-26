accessing
messages
	messages notNil ifTrue: [^messages].
	argumentSearches notEmpty ifTrue: [^messages := #()].
	messages := Set new.
	searches do: 
			[:each | 
			| searchMessages |
			searchMessages := each sentMessages.
			RBProgramNode optimizedSelectors 
				do: [:sel | searchMessages remove: sel ifAbsent: []].
			searchMessages isEmpty ifTrue: [^messages := #()].
			messages addAll: searchMessages].
	^messages := messages asArray