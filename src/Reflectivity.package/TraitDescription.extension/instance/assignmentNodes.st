*Reflectivity
assignmentNodes
	^self methods flatCollect: [ :each | each assignmentNodes ]