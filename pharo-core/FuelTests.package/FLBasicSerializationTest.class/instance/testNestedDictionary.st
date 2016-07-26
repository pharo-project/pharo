tests-collections
testNestedDictionary

	| childDic dic |
	childDic := Dictionary new.
	childDic at: #child1 put: 'abcde'.
	dic := Dictionary new.
	dic at: #parent1 put: 'sample string.'.
	dic at: #parent2 put: 100.
	dic at: #parent3 put: #(10 20 30 ).
	dic at: #parent4 put: childDic.
	
	self assertSerializationEqualityOf: childDic.
	self assertSerializationEqualityOf: dic.