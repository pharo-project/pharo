instance creation
uniqueInstance

	^ uniqueInstance ifNil: [ uniqueInstance := self new ].