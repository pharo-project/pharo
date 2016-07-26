instance creation
getterMethod: aVarName 
	^(self new)
		matchesMethod: '`method ^' , aVarName do: [:aNode :ans | aNode selector];
		yourself