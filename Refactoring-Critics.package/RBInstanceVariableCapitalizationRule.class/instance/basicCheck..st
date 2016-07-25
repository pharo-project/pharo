running
basicCheck: aClass 
	^ (aClass instVarNames allSatisfy: [ :each | 
		each first isLowercase ]) not