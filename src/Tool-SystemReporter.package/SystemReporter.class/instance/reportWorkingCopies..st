reporting
reportWorkingCopies: aStream
	| list |
	self header: 'Monticello Working Copies' on: aStream.
	list := MCWorkingCopy allManagers asSortedCollection: [:a :b | a name <= b name]  .
	list do: [:each | aStream nextPutAll: each description; cr]