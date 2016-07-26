testing
isUnchangedMethod

	^ self isMethodDefinedInImage 
		and: [self string withBlanksCondensed 
				= (self methodClass sourceCodeAt: self methodSelector) asString withBlanksCondensed ]