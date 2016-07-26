A RBDeprecateMethodRefactoring is a class, that represents functionality of deprecate refactoring.

Instance Variables
	oldSelector	:	<Object>
	newSelector:		<Object>

oldSelector
	- is a selector which shouldn't be used anymore, it's deprecated
newSelector
	- is a selector which will be used instead of a deprecated one
	
Note: 
It is recommended to use this refactoring only if number of arguments is either equal in both selectors, or the new one has no arguments at all.
If new selector has fewer number of arguments than the old one, it may lead to unexpected results.
If you use it in other conditions an error may be be occured.