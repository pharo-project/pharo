accessing
bundle
	^ self postpone: (self bundle: (postponedEvents ifNil: [ OrderedCollection new ]))