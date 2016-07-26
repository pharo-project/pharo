instance creation
uniqueInstance
	^uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]