testing
hasRemoved: aSelector 
	^removedMethods notNil and: [removedMethods includes: aSelector]