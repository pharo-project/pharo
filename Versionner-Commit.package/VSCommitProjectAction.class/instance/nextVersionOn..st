accessing
nextVersionOn: aUnit 
	^ nextVersion ifNil: [ aUnit commitVersion nextVersionOf: self project ]