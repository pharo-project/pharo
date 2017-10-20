*Reflectivity
sendNodes
	^self methods flatCollect: [ :each | each sendNodes ]