executing
executeOn: aUnit
	| version |
	version := self nextVersionOn: aUnit.
	self project devWorkflow 
		releaseFixedDevelopment: version
		description: aUnit comment.
	self 
		commitPackage: self packageName 
		comment: aUnit comment