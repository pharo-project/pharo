*Reflectivity
astNodes
	self flag: #FIXME. "will return all temps with the same name... as we do not allow shadowing this is ok but shoudl be fixed later"
	^self method variableNodes select: [ :each | (each name = self name)]