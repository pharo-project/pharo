testing
isProvided: marker
	marker ifNil: [^ true].
	^ (self isRequired: marker) not and: [(self isDisabled: marker) not]