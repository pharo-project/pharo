testing
isUserSelection
	^ self eventType notNil and: [ (self eventType = #unknown) not ]