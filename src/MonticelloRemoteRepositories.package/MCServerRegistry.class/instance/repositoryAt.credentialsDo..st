private
repositoryAt: urlString credentialsDo: aBlock


	| possibleMatches bestMatch |
	possibleMatches := registry associations select: [ :e | urlString beginsWith: e key ].
	possibleMatches isEmpty ifTrue: [ ^ aBlock value: '' value: '' ].
	bestMatch := possibleMatches inject: possibleMatches anyOne into: [ :last :new | 
		((new key asUrl path size > last key asUrl path size) 
		"this is ugly, but URL always returns a path element, even an empty one..."
		or: [ last key asUrl path size = 1 and: [ last key asUrl path last isEmpty ]]) 
			ifTrue: [ new ]
			ifFalse: [ last ]].
	^ aBlock value: bestMatch value username value: bestMatch value password.