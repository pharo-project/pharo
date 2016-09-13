assertions
assertSelector: aSelector enumerates: aCollection from: aDictionary
	| enumerated |

	enumerated := OrderedCollection new.
	aDictionary perform: aSelector with: [:each | enumerated add: each].
	self assert: enumerated asArray = aCollection asArray.