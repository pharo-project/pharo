api
retrieveSpec: aSelector

	self layout ifNil: [ ^ super retrieveSpec: aSelector ].

	^ self layout
		selector: aSelector;
		yourself