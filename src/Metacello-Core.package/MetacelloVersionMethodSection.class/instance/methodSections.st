accessing
methodSections

	methodSections ifNil: [ methodSections := OrderedCollection new ].
	^methodSections