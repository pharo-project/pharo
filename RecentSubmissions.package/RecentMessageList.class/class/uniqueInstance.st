instance creation
uniqueInstance

	^UniqueInstance ifNil: [UniqueInstance := self basicNew initialize]