testing
testAllSendersOfReturnsExistingMethods
	"Non-regression test for
	http://code.google.com/p/pharo/issues/detail?id=2593 "
	| senders |
	senders := self systemNavigationToTest allSendersOf: #methodsFor:.
	senders
		do: [:aReference | self
				assert: (aReference actualClass canUnderstand: aReference selector)]