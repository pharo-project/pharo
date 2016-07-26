sorted collections for testing
sortedCollectionForTestingWithClassVariable.
	^  SortedCollection sortBlock: [:a :b | 
		ClassVariableForTesting 
			ifTrue: [ a <= b ] 
			ifFalse: [ a >= b ] ]. 
