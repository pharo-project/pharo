accessing
replace: code with: newCode in: aParseTree 
	^(self 
		replace: code
		with: newCode
		method: false)
		executeTree: aParseTree;
		tree