running
sortedBlockClosureWithInstanceVariable
	^ SortedCollection
		sortBlock: [ :a :b | 
			instanceVariableForTesting
				ifTrue: [ a <= b ]
				ifFalse: [ a >= b ] ]