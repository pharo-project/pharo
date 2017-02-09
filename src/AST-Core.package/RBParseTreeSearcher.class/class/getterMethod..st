instance creation
getterMethod: aVarName 
	^(self new)
		matchesMethod: '`method ^' , aVarName do: [:aNode :ans | aNode selector];
		matchesMethod: '`method ^' , aVarName, ' ifNil:[ `@expr]' do: [:aNode :ans | aNode selector];
		yourself