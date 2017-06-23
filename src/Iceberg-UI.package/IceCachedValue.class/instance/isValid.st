accessing
isValid
	^ lastUpdated isNotNil and: [ (DateAndTime current - lastUpdated) < self class timeout ]