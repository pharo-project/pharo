private-replacing
isContainmentReplacement: aNode 
	^(self mappingFor: self receiver) = aNode 
		or: [self arguments anySatisfy: [:each | (self mappingFor: each) = aNode]]