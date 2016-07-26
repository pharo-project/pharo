accessing
findRealmFor: anAuthHeader
	(anAuthHeader substrings: ',') do: [ :token |
		"String>>trimBoth: is a grease method, so not sure if we should be using it."
		(token includesSubstring: 'realm=') 
			ifTrue: [ ^ ((token substrings: '=') second) ] ].
	^ nil