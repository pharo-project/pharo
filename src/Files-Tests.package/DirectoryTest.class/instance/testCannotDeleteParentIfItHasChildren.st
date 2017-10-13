tests
testCannotDeleteParentIfItHasChildren

	| dir childName child |
	dir := Directory named: 'testtesttest'.
	dir create.
	
	childName := dir name, Directory pathDelimiter asString, 'childDir'.
	child := Directory named: childName. 
	child create.

	self should: [ dir delete ] raise: Error.
	child delete.
	dir delete.