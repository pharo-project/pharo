accessing
layoutClass
	^ layoutClass ifNil: [ builder classNamed: #FixedLayout ]