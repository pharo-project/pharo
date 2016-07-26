accessing
instance
	^ Instance ifNil: [ Instance := self basicNew ]