instance creation
returnSetterMethod: aVarName 
	^(self new)
		matchesMethod: '`method: `Arg ^' , aVarName , ' := `Arg'
			do: [:aNode :ans | aNode selector];
		yourself