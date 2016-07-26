instance creation
instance
	^ Instance ifNil: [ Instance := self basicNew initialize ]