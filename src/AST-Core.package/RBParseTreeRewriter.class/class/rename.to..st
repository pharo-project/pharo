instance creation
rename: varName to: newVarName 
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule
		replace: varName with: newVarName;
		replaceArgument: varName with: newVarName.
	^rewriteRule