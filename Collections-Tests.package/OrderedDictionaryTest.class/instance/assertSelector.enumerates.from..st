assertions
assertSelector: aSelector enumerates: aCollection from: aDictionary
	| enumerated |

	enumerated := OrderedCollection new.
	self emptyDictionary perform: aSelector with: [:each | enumerated add: each].
	self assert: enumerated isEmpty.

	aDictionary perform: aSelector with: [:each | enumerated add: each].
	self assert: enumerated asArray = aCollection asArray.