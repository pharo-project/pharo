running
checkClass: aClass 
	aClass instVarNames do: [ :each | 
		each first isLowercase ifFalse: [
			result 
				addClass: aClass
				instanceVariable: each ] ]