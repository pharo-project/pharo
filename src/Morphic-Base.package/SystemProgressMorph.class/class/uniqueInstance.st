instance creation
uniqueInstance

	^ UniqueInstance ifNil: [ UniqueInstance := self new ].