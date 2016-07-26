*Reflectivity
instanceVariableWriteNodes
	^self methods flatCollect: [ :each | each instanceVariableWriteNodes ]