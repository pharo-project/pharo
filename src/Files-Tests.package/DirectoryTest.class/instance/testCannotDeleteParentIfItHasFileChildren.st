tests
testCannotDeleteParentIfItHasFileChildren

	| dir childName child |
	dir := Directory named: 'testtesttest'.
	dir create.
	
	childName := dir name, Directory pathDelimiter asString, 'childFile'.
	child := File named: childName. 
	child writeStream close.

	self should: [ dir delete ] raise: Error.
	child delete.
	dir delete.