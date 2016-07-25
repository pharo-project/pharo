private
packageOf: aCritc
	^ aCritc mcWorkingCopy 
	"aCritc isCompiledMethod 
		ifTrue: [MCWorkingCopy managersForClass: aCritc methodClass selector: aCritc selector do: [ :package | ^ package ]]	
		ifFalse: [MCWorkingCopy managersForClass: aCritc do: [: package | ^ package ]]	"