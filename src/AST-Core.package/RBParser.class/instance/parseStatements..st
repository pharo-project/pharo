private-parsing
parseStatements: pragmaBoolean
	^ self parseStatements: pragmaBoolean into: self sequenceNodeClass new