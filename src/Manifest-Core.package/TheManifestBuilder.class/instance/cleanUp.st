adding/removing
cleanUp

	| tagfp |
	tagfp := self class falsePositiveBeginningTag.
	self manifest class methodsDo: [ :method |
		(method selector asString beginsWith: tagfp)
				ifTrue: [self removeObsoleteFalsePositiveOf: method] ]
	
	


